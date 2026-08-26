# -*- coding: UTF-8 -*-
# =============================================================================
# Carte Trello : "Analyser et convertir les données de durée"
#
# Objectifs :
#   1. Convertir les horaires HHMM en minutes depuis minuit.
#   2. Comparer le retard au départ observé au retard recalculé.
#   3. Classer les vols les plus retardés avec min_rank().
#   4. Comparer air_time à la durée apparente entre départ et arrivée.
#   5. Produire une visualisation simple de la comparaison des retards.
#
# Le script ne modifie pas les données sources et conserve les valeurs NA.
# MongoDB est utilisé en priorité ; data/flights.xlsx sert de solution de
# secours lorsque MongoDB n'est pas configuré ou n'est pas accessible.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# -----------------------------------------------------------------------------
# Paramètres et localisation du projet
# -----------------------------------------------------------------------------

mongo_uri <- Sys.getenv("MONGODB_URI")
mongo_db <- Sys.getenv("MONGO_CLEAN_DB", unset = "nyc_flights_cleaned")

arguments_complets <- commandArgs(trailingOnly = FALSE)
argument_fichier <- grep("^--file=", arguments_complets, value = TRUE)

if (file.exists(file.path(getwd(), "data", "flights.xlsx"))) {
  dossier_projet <- normalizePath(getwd(), mustWork = TRUE)
} else if (length(argument_fichier) > 0L) {
  chemin_script <- sub("^--file=", "", argument_fichier[[1]])
  dossier_projet <- normalizePath(
    file.path(dirname(chemin_script), ".."),
    mustWork = TRUE
  )
} else {
  dossier_projet <- normalizePath(file.path(getwd(), ".."), mustWork = TRUE)
}

fichier_vols <- file.path(dossier_projet, "data", "flights.xlsx")
dossier_sortie <- Sys.getenv(
  "OUTPUT_DIR",
  unset = file.path(dossier_projet, "output")
)

if (!dir.exists(dossier_sortie)) {
  dir.create(dossier_sortie, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Fonctions de chargement
# -----------------------------------------------------------------------------

lire_vols_mongodb <- function() {
  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("Le package 'mongolite' est nécessaire pour lire MongoDB.")
  }

  message("Lecture de ", mongo_db, ".flights...")
  connexion <- mongolite::mongo(
    collection = "flights",
    db = mongo_db,
    url = mongo_uri
  )

  connexion$find(
    query = "{}",
    fields = paste0(
      '{"_id":0,"year":1,"month":1,"day":1,',
      '"dep_time":1,"sched_dep_time":1,"dep_delay_min":1,',
      '"arr_time":1,"air_time_min":1,"carrier":1,"flight":1,',
      '"origin":1,"dest":1}'
    )
  ) |>
    as_tibble()
}

lire_vols_excel <- function(chemin) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Le package 'readxl' est nécessaire pour lire data/flights.xlsx. ",
      "Installez-le avec install.packages('readxl')."
    )
  }
  if (!file.exists(chemin)) {
    stop("Fichier introuvable : ", chemin)
  }

  message("Lecture de ", chemin, "...")

  # Le classeur fourni contient une ligne CSV complète dans chaque cellule
  # de sa première colonne. On lit donc les cellules, puis leur contenu CSV.
  lignes <- readxl::read_excel(
    chemin,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )[[1]]

  connexion_texte <- textConnection(lignes, encoding = "UTF-8")
  on.exit(close(connexion_texte), add = TRUE)

  read.csv(
    connexion_texte,
    header = TRUE,
    na.strings = c("", "NA", "NaN", "None", "null"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) |>
    as_tibble()
}

# Harmoniser les suffixes utilisés par la collection MongoDB nettoyée.
harmoniser_noms <- function(donnees) {
  correspondances <- c(
    dep_delay_min = "dep_delay",
    arr_delay_min = "arr_delay",
    air_time_min = "air_time",
    distance_mi = "distance"
  )

  for (ancien_nom in names(correspondances)) {
    nouveau_nom <- correspondances[[ancien_nom]]
    if (ancien_nom %in% names(donnees) && !nouveau_nom %in% names(donnees)) {
      names(donnees)[names(donnees) == ancien_nom] <- nouveau_nom
    }
  }

  donnees
}

if (nzchar(mongo_uri)) {
  flights <- tryCatch(
    lire_vols_mongodb(),
    error = function(erreur) {
      warning(
        "MongoDB n'a pas pu être lu (", conditionMessage(erreur),
        "). Utilisation de data/flights.xlsx."
      )
      lire_vols_excel(fichier_vols)
    }
  )
} else {
  flights <- lire_vols_excel(fichier_vols)
}

flights <- harmoniser_noms(flights)

colonnes_attendues <- c(
  "year", "month", "day", "dep_time", "sched_dep_time", "dep_delay",
  "arr_time", "air_time", "carrier", "flight", "origin", "dest"
)

colonnes_absentes <- setdiff(colonnes_attendues, names(flights))
if (length(colonnes_absentes) > 0L) {
  stop(
    "Colonnes attendues absentes : ",
    paste(colonnes_absentes, collapse = ", ")
  )
}

# Conserver uniquement les variables utiles à cette carte Trello.
flights <- flights |>
  select(all_of(colonnes_attendues))

if (nrow(flights) == 0L) {
  stop("Le jeu de données flights est vide.")
}

# =============================================================================
# 1. Conversion des horaires HHMM en minutes depuis minuit
# =============================================================================

convertir_hhmm_minutes <- function(horaire) {
  valeur <- suppressWarnings(as.integer(horaire))
  heures <- valeur %/% 100L
  minutes <- valeur %% 100L

  # 24 h 00 est une écriture valide de minuit et vaut 1 440 minutes.
  horaire_valide <- !is.na(valeur) & (
    (heures >= 0L & heures <= 23L & minutes >= 0L & minutes <= 59L) |
      valeur == 2400L
  )

  resultat <- rep(NA_integer_, length(valeur))
  resultat[horaire_valide] <-
    heures[horaire_valide] * 60L + minutes[horaire_valide]
  resultat
}

flights_durees <- flights |>
  mutate(
    date = as.Date(sprintf("%04d-%02d-%02d", year, month, day)),
    dep_minutes = convertir_hhmm_minutes(dep_time),
    sched_dep_minutes = convertir_hhmm_minutes(sched_dep_time),
    arr_minutes = convertir_hhmm_minutes(arr_time)
  )

exemples_conversion <- tibble(
  horaire_hhmm = c(530L, 1430L, 5L, 2400L, NA_integer_)
) |>
  mutate(minutes_depuis_minuit = convertir_hhmm_minutes(horaire_hhmm))

message("\n============================================================")
message("ANALYSE ET CONVERSION DES DONNÉES DE DURÉE")
message("============================================================")
message("Nombre total de vols : ", format(nrow(flights_durees), big.mark = " "))
message("\n--- Exemples de conversion HHMM -> minutes depuis minuit ---")
print(exemples_conversion, n = Inf, width = Inf)

# =============================================================================
# 2. Comparaison entre dep_delay et le retard recalculé
# =============================================================================

corriger_passage_minuit <- function(ecart_minutes) {
  case_when(
    is.na(ecart_minutes) ~ NA_real_,
    ecart_minutes < -720 ~ ecart_minutes + 1440,
    ecart_minutes > 720 ~ ecart_minutes - 1440,
    TRUE ~ as.numeric(ecart_minutes)
  )
}

# La correction choisit l'écart d'horloge le plus proche entre les deux
# horaires. Elle corrige notamment 00:10 - 23:50 en +20 minutes.
flights_durees <- flights_durees |>
  mutate(
    ecart_depart_brut = dep_minutes - sched_dep_minutes,
    retard_depart_calcule = corriger_passage_minuit(ecart_depart_brut),
    ecart_retard = retard_depart_calcule - dep_delay,
    statut_comparaison = case_when(
      is.na(ecart_retard) ~ NA_character_,
      ecart_retard == 0 ~ "Correspondance exacte",
      abs(ecart_retard) <= 1 ~ "Correspondance à 1 minute près",
      TRUE ~ "Incohérence à examiner"
    )
  )

comparaison_depart <- flights_durees |>
  filter(!is.na(dep_delay), !is.na(retard_depart_calcule))

resume_comparaison_depart <- comparaison_depart |>
  summarise(
    lignes_comparables = n(),
    correspondances_exactes = sum(ecart_retard == 0),
    correspondances_presque_exactes = sum(
      abs(ecart_retard) > 0 & abs(ecart_retard) <= 1
    ),
    correspondances_exactes_ou_presque = sum(abs(ecart_retard) <= 1),
    incoherences = sum(abs(ecart_retard) > 1),
    taux_coherence_pct = round(
      100 * correspondances_exactes_ou_presque / n(),
      3
    )
  )

exemples_incoherences <- comparaison_depart |>
  filter(abs(ecart_retard) > 1) |>
  arrange(desc(abs(ecart_retard))) |>
  select(
    date, carrier, flight, origin, dest, dep_time, sched_dep_time,
    dep_delay, retard_depart_calcule, ecart_retard
  ) |>
  slice_head(n = 10)

message("\n--- Comparaison dep_delay / retard recalculé ---")
print(resume_comparaison_depart, width = Inf)
message(sprintf(
  "Taux de correspondance à une minute près : %.3f %%",
  resume_comparaison_depart$taux_coherence_pct
))

if (nrow(exemples_incoherences) == 0L) {
  message("Aucune incohérence supérieure à une minute n'a été détectée.")
} else {
  message("\nExemples d'écarts à examiner :")
  print(exemples_incoherences, n = Inf, width = Inf)
  message(
    paste0(
      "Ces écarts peuvent notamment concerner des retards supérieurs à ",
      "12 heures. Deux horaires HHMM seuls ne permettent pas de savoir avec ",
      "certitude si le départ réel appartient au même jour ou au jour suivant."
    )
  )
}

# =============================================================================
# 3. Les 10 premiers rangs de retard avec min_rank()
# =============================================================================

vols_plus_retardes <- flights_durees |>
  filter(!is.na(dep_delay)) |>
  mutate(rang_retard = min_rank(desc(dep_delay))) |>
  filter(rang_retard <= 10) |>
  arrange(rang_retard, desc(dep_delay), date, carrier, flight) |>
  select(
    rang_retard, date, carrier, flight, origin, dest, dep_delay
  )

message("\n--- Vols des 10 premiers rangs de retard au départ ---")
message(
  nrow(vols_plus_retardes),
  " ligne(s) conservée(s), ex aequo inclus conformément à min_rank()."
)
print(vols_plus_retardes, n = Inf, width = Inf)

# =============================================================================
# 4. Comparaison entre air_time et la durée apparente arr_time - dep_time
# =============================================================================

flights_durees <- flights_durees |>
  mutate(
    duree_apparente_naive = arr_minutes - dep_minutes,
    duree_apparente_corrigee = case_when(
      is.na(duree_apparente_naive) ~ NA_real_,
      duree_apparente_naive < 0 ~ duree_apparente_naive + 1440,
      TRUE ~ as.numeric(duree_apparente_naive)
    ),
    ecart_duree_air = duree_apparente_corrigee - air_time
  )

comparaison_air_time <- flights_durees |>
  filter(!is.na(air_time), !is.na(duree_apparente_corrigee))

resume_comparaison_air_time <- comparaison_air_time |>
  summarise(
    lignes_comparables = n(),
    air_time_moyen_min = round(mean(air_time), 1),
    duree_apparente_moyenne_min = round(mean(duree_apparente_corrigee), 1),
    ecart_moyen_min = round(mean(ecart_duree_air), 1),
    ecart_median_min = round(median(ecart_duree_air), 1),
    ecart_absolu_moyen_min = round(mean(abs(ecart_duree_air)), 1)
  )

message("\n--- Comparaison air_time / durée apparente ---")
print(resume_comparaison_air_time, width = Inf)
message(
  paste0(
    "Intuitivement, on pourrait attendre que arr_time - dep_time représente ",
    "la durée du vol. La correction appliquée évite seulement les durées ",
    "négatives dues au passage à minuit."
  )
)
message(
  paste0(
    "Cette durée reste cependant une durée apparente : les horaires de départ ",
    "et d'arrivée sont exprimés dans les heures locales des aéroports, et le ",
    "jeu de données utilisé ici ne fournit pas directement le décalage horaire ",
    "à appliquer à chaque vol. Elle peut aussi inclure des temps au sol."
  )
)
message(
  paste0(
    "air_time est donc la variable la plus appropriée pour mesurer le temps ",
    "réellement passé en vol. Aucune correction de fuseau horaire n'est ",
    "inventée dans cette analyse."
  )
)

# =============================================================================
# 5. Visualisation du retard observé et du retard recalculé
# =============================================================================

# Échantillonner les correspondances cohérentes pour garder un graphique
# lisible, tout en conservant toutes les incohérences détectées.
set.seed(1L)
comparaisons_coherentes <- comparaison_depart |>
  filter(abs(ecart_retard) <= 1)
comparaisons_incoherentes <- comparaison_depart |>
  filter(abs(ecart_retard) > 1)

echantillon_coherent <- comparaisons_coherentes |>
  slice_sample(n = min(20000L, nrow(comparaisons_coherentes)))

donnees_graphique <- bind_rows(
  echantillon_coherent,
  comparaisons_incoherentes
) |>
  mutate(
    type_comparaison = if_else(
      abs(ecart_retard) <= 1,
      "Correspondance a 1 min pres",
      "Ecart superieur a 1 min"
    )
  )

graphique_comparaison <- ggplot(
  donnees_graphique,
  aes(
    x = dep_delay,
    y = retard_depart_calcule,
    color = type_comparaison
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "#475569"
  ) +
  geom_point(alpha = 0.35, size = 1) +
  scale_color_manual(values = c(
    "Correspondance a 1 min pres" = "#2563EB",
    "Ecart superieur a 1 min" = "#DC2626"
  )) +
  labs(
    title = "Retard au depart observe et recalcule",
    subtitle = paste0(
      "Echantillon de ", format(nrow(echantillon_coherent), big.mark = " "),
      " correspondances et toutes les incoherences"
    ),
    x = "Retard observe dep_delay (minutes)",
    y = "Retard recalcule (minutes)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

fichier_graphique <- file.path(
  dossier_sortie,
  "comparaison_retard_depart_calcule.png"
)

ggsave(
  fichier_graphique,
  graphique_comparaison,
  width = 9,
  height = 6,
  dpi = 150
)

message("\nGraphique enregistré dans : ", normalizePath(fichier_graphique))

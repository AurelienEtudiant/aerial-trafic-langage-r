# -*- coding: UTF-8 -*-
# =============================================================================
# Analyse des retards au départ et à l'arrivée
#
# Ce script répond uniquement aux questions 1 à 4 de la section 3.2 :
#   1. vols les plus retardés ;
#   2. retard moyen au départ ;
#   3. retards particuliers ;
#   4. vols partis ou arrivés en avance.
#
# Source utilisée en priorité : collection MongoDB nettoyée par clean_data.R.
# Si MongoDB n'est pas configuré, le script lit data/flights.xlsx sans modifier
# le classeur. Les graphiques sont enregistrés dans le dossier output/.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# -----------------------------------------------------------------------------
# Paramètres et fonctions de chargement
# -----------------------------------------------------------------------------

mongo_uri <- Sys.getenv("MONGODB_URI")
mongo_db <- Sys.getenv("MONGO_CLEAN_DB", unset = "nyc_flights_cleaned")

# Retrouver la racine du projet, que le script soit lancé avec Rscript ou sourcé.
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
      '{"_id":0,"year":1,"month":1,"day":1,"dep_time":1,',
      '"sched_dep_time":1,"dep_delay_min":1,"arr_time":1,',
      '"sched_arr_time":1,"arr_delay_min":1,"carrier":1,',
      '"flight":1,"tailnum":1,"origin":1,"dest":1}'
    )
  ) |>
    as_tibble() |>
    rename(
      dep_delay = dep_delay_min,
      arr_delay = arr_delay_min
    )
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

  # Le classeur fourni contient une ligne CSV par cellule dans une seule colonne.
  lignes <- readxl::read_excel(
    chemin,
    col_names = FALSE,
    col_types = "text"
  )[[1]]

  connexion_texte <- textConnection(lignes, encoding = "UTF-8")
  on.exit(close(connexion_texte), add = TRUE)

  read.csv(
    connexion_texte,
    header = TRUE,
    na.strings = c("", "NA", "NaN"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) |>
    as_tibble()
}

# Réutiliser la collection nettoyée si elle est disponible. En l'absence de
# configuration MongoDB, utiliser directement le fichier fourni dans data/.
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

# Harmoniser les types utiles à l'analyse. Les NA sont conservés dans flights.
colonnes_numeriques <- c(
  "year", "month", "day", "dep_time", "sched_dep_time", "dep_delay",
  "arr_time", "sched_arr_time", "arr_delay", "flight"
)

flights <- flights |>
  mutate(
    across(any_of(colonnes_numeriques), ~ suppressWarnings(as.numeric(.x))),
    date_vol = as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  )

colonnes_vol <- c(
  "date_vol", "carrier", "flight", "tailnum", "origin", "dest",
  "dep_time", "sched_dep_time", "dep_delay",
  "arr_time", "sched_arr_time", "arr_delay"
)

# Contrôle explicite des valeurs manquantes. Elles restent présentes dans le
# jeu de données ; na.rm = TRUE est utilisé seulement dans les calculs concernés.
resume_na <- flights |>
  summarise(
    nombre_vols = n(),
    dep_delay_manquants = sum(is.na(dep_delay)),
    arr_delay_manquants = sum(is.na(arr_delay))
  )

message("\n--- Contrôle des valeurs manquantes ---")
print(resume_na)

# =============================================================================
# 1. Vols les plus retardés
# =============================================================================

# slice_max(..., with_ties = TRUE) conserve les éventuels ex aequo à la
# dixième place.
vols_plus_retardes_arrivee <- flights |>
  filter(!is.na(arr_delay)) |>
  slice_max(arr_delay, n = 10, with_ties = TRUE) |>
  arrange(desc(arr_delay)) |>
  select(all_of(colonnes_vol))

vols_plus_retardes_depart <- flights |>
  filter(!is.na(dep_delay)) |>
  slice_max(dep_delay, n = 10, with_ties = TRUE) |>
  arrange(desc(dep_delay)) |>
  select(all_of(colonnes_vol))

# Un retard strictement positif signifie que le vol est parti/arrivé après
# l'horaire prévu. Pour classer les vols retardés aux deux étapes, on utilise
# la somme des deux retards, en minutes.
vols_retardes_depart_et_arrivee <- flights |>
  filter(
    !is.na(dep_delay), !is.na(arr_delay),
    dep_delay > 0, arr_delay > 0
  ) |>
  mutate(retard_total = dep_delay + arr_delay) |>
  slice_max(retard_total, n = 10, with_ties = TRUE) |>
  arrange(desc(retard_total)) |>
  select(all_of(colonnes_vol), retard_total)

message("\n--- Top des retards à l'arrivée ---")
print(vols_plus_retardes_arrivee, n = Inf, width = Inf)

message("\n--- Top des retards au départ ---")
print(vols_plus_retardes_depart, n = Inf, width = Inf)

message("\n--- Top des retards à la fois au départ et à l'arrivée ---")
print(vols_retardes_depart_et_arrivee, n = Inf, width = Inf)

# =============================================================================
# 2. Retard moyen au départ
# =============================================================================

# Les valeurs négatives (départs en avance) font partie de la moyenne, comme
# dans la définition de dep_delay. Seules les valeurs NA sont ignorées.
retard_moyen_depart <- flights |>
  summarise(
    nombre_vols_renseignes = sum(!is.na(dep_delay)),
    retard_moyen_depart = round(mean(dep_delay, na.rm = TRUE), 2)
  )

retard_moyen_journalier_depart <- flights |>
  group_by(date_vol) |>
  summarise(
    nombre_vols_renseignes = sum(!is.na(dep_delay)),
    retard_moyen_depart = if_else(
      nombre_vols_renseignes > 0,
      round(mean(dep_delay, na.rm = TRUE), 2),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  arrange(date_vol)

message("\n--- Retard moyen au départ pour l'ensemble des vols ---")
print(retard_moyen_depart)

message("\n--- Retard moyen journalier au départ ---")
print(retard_moyen_journalier_depart, n = Inf)

# =============================================================================
# 3. Retards particuliers
# =============================================================================

# "Parti à l'heure" est défini par dep_delay <= 0 : le vol est parti à
# l'heure exacte (0) ou en avance (valeur négative), donc sans retard au départ.
vols_arrivee_plus_2h_partis_a_heure <- flights |>
  filter(
    !is.na(dep_delay), !is.na(arr_delay),
    dep_delay <= 0,
    arr_delay > 120
  ) |>
  arrange(desc(arr_delay)) |>
  select(all_of(colonnes_vol))

# Les deux retards doivent être renseignés pour pouvoir affirmer que le vol
# n'a dépassé deux heures ni au départ ni à l'arrivée.
vols_sans_retard_plus_2h <- flights |>
  filter(
    !is.na(dep_delay), !is.na(arr_delay),
    dep_delay <= 120,
    arr_delay <= 120
  ) |>
  arrange(desc(dep_delay), desc(arr_delay)) |>
  select(all_of(colonnes_vol))

message(
  "\nVols arrivés avec plus de 2 h de retard après un départ à l'heure : ",
  nrow(vols_arrivee_plus_2h_partis_a_heure)
)
print(head(vols_arrivee_plus_2h_partis_a_heure, 20), width = Inf)

message(
  "\nVols sans retard supérieur à 2 h au départ ni à l'arrivée : ",
  nrow(vols_sans_retard_plus_2h)
)
print(head(vols_sans_retard_plus_2h, 20), width = Inf)

# =============================================================================
# 4. Vols en avance
# =============================================================================

# Une valeur négative indique une avance par rapport à l'horaire prévu.
vols_partis_en_avance <- flights |>
  filter(!is.na(dep_delay), dep_delay < 0) |>
  arrange(dep_delay) |>
  select(all_of(colonnes_vol))

vols_arrives_en_avance <- flights |>
  filter(!is.na(arr_delay), arr_delay < 0) |>
  arrange(arr_delay) |>
  select(all_of(colonnes_vol))

message("\nVols partis en avance : ", nrow(vols_partis_en_avance))
print(head(vols_partis_en_avance, 20), width = Inf)

message("\nVols arrivés en avance : ", nrow(vols_arrives_en_avance))
print(head(vols_arrives_en_avance, 20), width = Inf)

# =============================================================================
# Visualisations
# =============================================================================

# Les histogrammes sont zoomés entre les 1er et 99e percentiles pour éviter
# que quelques retards extrêmes écrasent visuellement le reste. coord_cartesian
# ne retire pas ces vols des données ni des calculs.
limites_depart <- quantile(
  flights$dep_delay,
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  names = FALSE
)
limites_arrivee <- quantile(
  flights$arr_delay,
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  names = FALSE
)

graphique_depart <- flights |>
  filter(!is.na(dep_delay)) |>
  ggplot(aes(x = dep_delay)) +
  geom_histogram(binwidth = 5, fill = "#2563EB", color = "white") +
  coord_cartesian(xlim = limites_depart) +
  labs(
    title = "Distribution des retards au depart",
    subtitle = "Zoom entre les 1er et 99e percentiles",
    x = "Retard au depart (minutes)",
    y = "Nombre de vols"
  ) +
  theme_minimal(base_size = 12)

graphique_arrivee <- flights |>
  filter(!is.na(arr_delay)) |>
  ggplot(aes(x = arr_delay)) +
  geom_histogram(binwidth = 5, fill = "#E11D48", color = "white") +
  coord_cartesian(xlim = limites_arrivee) +
  labs(
    title = "Distribution des retards a l'arrivee",
    subtitle = "Zoom entre les 1er et 99e percentiles",
    x = "Retard a l'arrivee (minutes)",
    y = "Nombre de vols"
  ) +
  theme_minimal(base_size = 12)

# Ajouter les dates absentes avec NA empêche ggplot de relier artificiellement
# deux périodes éloignées dans le temps.
calendrier_complet <- data.frame(
  date_vol = seq(
    min(retard_moyen_journalier_depart$date_vol),
    max(retard_moyen_journalier_depart$date_vol),
    by = "day"
  )
)

donnees_graphique_journalier <- calendrier_complet |>
  left_join(retard_moyen_journalier_depart, by = "date_vol")

graphique_moyenne_journaliere <- donnees_graphique_journalier |>
  ggplot(aes(x = date_vol, y = retard_moyen_depart)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dashed") +
  geom_line(color = "#059669", linewidth = 0.8) +
  labs(
    title = "Evolution du retard moyen journalier au depart",
    x = "Date",
    y = "Retard moyen au depart (minutes)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(dossier_sortie, "distribution_retards_depart.png"),
  graphique_depart,
  width = 9, height = 6, dpi = 150
)
ggsave(
  file.path(dossier_sortie, "distribution_retards_arrivee.png"),
  graphique_arrivee,
  width = 9, height = 6, dpi = 150
)
ggsave(
  file.path(dossier_sortie, "retard_moyen_journalier_depart.png"),
  graphique_moyenne_journaliere,
  width = 10, height = 6, dpi = 150
)

message("\nGraphiques enregistrés dans : ", normalizePath(dossier_sortie))

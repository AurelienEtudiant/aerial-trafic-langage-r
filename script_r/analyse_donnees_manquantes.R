# -*- coding: UTF-8 -*-
# =============================================================================
# Analyse des données manquantes dans les vols
#
# Ce script répond à la partie 3.4 consacrée à l'identification des NA.
# Il ne supprime et ne remplace aucune valeur manquante.
#
# Source utilisée en priorité : collection MongoDB nettoyée par clean_data.R.
# Si MongoDB n'est pas configuré, le script lit data/flights.xlsx sans modifier
# le classeur. Le graphique est enregistré dans le dossier output/.
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
# Chargement des données
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

  # Sélectionner les variables métier correspondant aux 19 colonnes du fichier
  # Excel. Les indicateurs techniques ajoutés par clean_data.R ne sont pas des
  # variables de vol originales et ne sont donc pas mélangés à ce bilan.
  connexion$find(
    query = "{}",
    fields = paste0(
      '{"_id":0,"year":1,"month":1,"day":1,"dep_time":1,',
      '"sched_dep_time":1,"dep_delay_min":1,"arr_time":1,',
      '"sched_arr_time":1,"arr_delay_min":1,"carrier":1,',
      '"flight":1,"tailnum":1,"origin":1,"dest":1,',
      '"air_time_min":1,"distance_mi":1,"hour":1,"minute":1,',
      '"scheduled_departure":1}'
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

  # Le classeur fourni contient une ligne CSV par cellule dans une seule colonne.
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

# Harmoniser les noms différents entre la collection nettoyée et le fichier.
harmoniser_noms <- function(donnees) {
  correspondances <- c(
    dep_delay_min = "dep_delay",
    arr_delay_min = "arr_delay",
    air_time_min = "air_time",
    distance_mi = "distance",
    scheduled_departure = "time_hour"
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
  "arr_time", "sched_arr_time", "arr_delay", "carrier", "flight",
  "tailnum", "origin", "dest", "air_time", "distance", "hour",
  "minute", "time_hour"
)

colonnes_absentes <- setdiff(colonnes_attendues, names(flights))
if (length(colonnes_absentes) > 0L) {
  stop(
    "Colonnes attendues absentes : ",
    paste(colonnes_absentes, collapse = ", ")
  )
}

# Conserver un même périmètre de variables dans les deux sources.
flights <- flights |> select(all_of(colonnes_attendues))

if (nrow(flights) == 0L) {
  stop("Le jeu de données flights est vide.")
}

# =============================================================================
# 1. Valeurs manquantes sur les variables principales
# =============================================================================

nombre_vols <- nrow(flights)
colonnes_principales <- c("dep_time", "dep_delay", "arr_time", "arr_delay")

# summarise() et across() permettent de compter les NA de toutes les colonnes
# sans modifier le jeu de données original.
comptage_na <- flights |>
  summarise(across(everything(), ~ sum(is.na(.x))))

tableau_na <- tibble(
  variable = names(comptage_na),
  nombre_na = as.integer(unlist(comptage_na[1, ], use.names = FALSE))
) |>
  mutate(
    pourcentage_na = round(100 * nombre_na / nombre_vols, 3)
  )

tableau_na_principales <- tableau_na |>
  filter(variable %in% colonnes_principales) |>
  mutate(ordre = match(variable, colonnes_principales)) |>
  arrange(ordre) |>
  select(-ordre)

message("\n============================================================")
message("ANALYSE DES DONNÉES MANQUANTES")
message("============================================================")
message("Nombre total de vols : ", format(nombre_vols, big.mark = " "))

message("\n--- Variables principales ---")
print(tableau_na_principales, n = Inf, width = Inf)

# =============================================================================
# 2. Toutes les autres colonnes contenant des NA
# =============================================================================

tableau_na_toutes <- tableau_na |>
  filter(nombre_na > 0L) |>
  arrange(desc(pourcentage_na), desc(nombre_na), variable)

tableau_autres_variables_na <- tableau_na_toutes |>
  filter(!variable %in% colonnes_principales)

message("\n--- Autres variables contenant des NA ---")
if (nrow(tableau_autres_variables_na) == 0L) {
  message("Aucune autre variable ne contient de NA.")
} else {
  print(tableau_autres_variables_na, n = Inf, width = Inf)
}

message("\n--- Toutes les variables contenant des NA ---")
print(tableau_na_toutes, n = Inf, width = Inf)

# =============================================================================
# 3. Interprétation métier, sans traitement automatique
# =============================================================================

profils_temps_manquants <- flights |>
  summarise(
    dep_et_arr_time_manquants = sum(is.na(dep_time) & is.na(arr_time)),
    dep_time_seul_manquant = sum(is.na(dep_time) & !is.na(arr_time)),
    arr_time_seul_manquant = sum(!is.na(dep_time) & is.na(arr_time)),
    arr_delay_manquant_arr_time_present = sum(
      is.na(arr_delay) & !is.na(arr_time)
    )
  )

message("\n--- Principaux constats ---")
message(
  "* Les NA sont uniquement recensés : aucune ligne ni valeur n'est supprimée."
)
message(sprintf(
  paste0(
    "* %s vols ont dep_time et arr_time manquants simultanément. ",
    "Ils peuvent correspondre à des vols potentiellement annulés, mais cette ",
    "hypothèse devra être confirmée dans l'analyse dédiée aux annulations."
  ),
  format(profils_temps_manquants$dep_et_arr_time_manquants, big.mark = " ")
))
message(sprintf(
  paste0(
    "* %s vols ont uniquement arr_time manquant alors que dep_time est ",
    "renseigné : il s'agit de données d'arrivée incomplètes, pas ",
    "automatiquement de vols annulés."
  ),
  format(profils_temps_manquants$arr_time_seul_manquant, big.mark = " ")
))
message(sprintf(
  paste0(
    "* %s vols ont arr_delay manquant malgré un arr_time renseigné. ",
    "Cela montre que toutes les absences ne suivent pas exactement le même ",
    "profil et qu'une interprétation métier est nécessaire."
  ),
  format(
    profils_temps_manquants$arr_delay_manquant_arr_time_present,
    big.mark = " "
  )
))

# =============================================================================
# 4. Visualisation du pourcentage de NA par variable
# =============================================================================

graphique_na <- tableau_na_toutes |>
  ggplot(aes(
    x = reorder(variable, pourcentage_na),
    y = pourcentage_na
  )) +
  geom_col(fill = "#7C3AED") +
  geom_text(
    aes(label = sprintf("%.2f %%", pourcentage_na)),
    hjust = -0.12,
    size = 3.8
  ) +
  coord_flip(clip = "off") +
  expand_limits(y = max(tableau_na_toutes$pourcentage_na) * 1.18) +
  labs(
    title = "Pourcentage de donnees manquantes par variable",
    x = "Variable",
    y = "Donnees manquantes (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 25, 10, 10)
  )

fichier_graphique <- file.path(
  dossier_sortie,
  "pourcentage_valeurs_manquantes.png"
)

ggsave(
  fichier_graphique,
  graphique_na,
  width = 9,
  height = 6,
  dpi = 150
)

message("\nGraphique enregistré dans : ", normalizePath(fichier_graphique))

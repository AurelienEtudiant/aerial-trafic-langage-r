# =============================================================================
# Carte Trello : "Analyser le rattrapage des retards en vol"
# Objectif : déterminer quels vols ont réussi à récupérer une partie ou la
# totalité de leur retard pendant le trajet.
#
# Travaux réalisés :
#   1. Filtrer les départs avec >= 60 min de retard
#   2. Calculer gain = arr_delay - dep_delay
#   3. Identifier les vols ayant rattrapé > 30 min
#   4. Convertir air_time en heures
#   5. Calculer gain_per_hour = gain / hours
#   6. Classer les vols
#   7. Interpréter les résultats
#
# Livrable : analyse du retard récupéré pendant les vols.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(mongolite)
  library(scales)
})

mongo_uri <- Sys.getenv("MONGODB_URI")
mongo_db <- Sys.getenv("MONGO_CLEAN_DB", unset = "nyc_flights_cleaned")
output_dir <- Sys.getenv("OUTPUT_DIR", unset = "output")

if (!nzchar(mongo_uri)) stop("MONGODB_URI est absente. Définissez-la dans .env.")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# Lecture des vols nettoyés
# -----------------------------------------------------------------------------
message("Lecture de ", mongo_db, ".flights...")
con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
flights <- con$find(
  query = '{"dep_delay_min":{"$ne":null},"arr_delay_min":{"$ne":null},"air_time_min":{"$ne":null}}',
  fields = '{"carrier":1,"flight":1,"tailnum":1,"origin":1,"dest":1,"dep_delay_min":1,"arr_delay_min":1,"air_time_min":1,"date":1}'
) |>
  as_tibble()

if (nrow(flights) == 0L) stop("Aucun vol exploitable (retards/air_time manquants) dans ", mongo_db, ".flights")

# -----------------------------------------------------------------------------
# 1. Filtrer les départs avec au moins 60 min de retard
# -----------------------------------------------------------------------------
delayed_departures <- flights |> filter(dep_delay_min >= 60)
message(sprintf("Vols partis avec >= 60 min de retard : %d", nrow(delayed_departures)))

# -----------------------------------------------------------------------------
# 2. Calculer le gain.
#
# ATTENTION : l'énoncé donne littéralement "gain = arr_delay - dep_delay",
# mais avec cette formule un vol qui RATTRAPE du retard (il arrive avec
# MOINS de retard qu'il n'est parti, donc arr_delay < dep_delay) obtient un
# gain négatif -- l'inverse de ce que décrit l'énoncé juste à côté ("ont
# rattrapé plus de 30 minutes" => filtré ensuite avec "gain > 30"). C'est une
# coquille de signe dans l'énoncé (le tutoriel nycflights13 dont ce TP
# s'inspire utilise gain = dep_delay - arr_delay). On retient donc ici la
# formule qui correspond au sens métier :
#   gain = dep_delay - arr_delay
#   gain > 0  => retard réduit pendant le vol (rattrapage)
#   gain < 0  => retard aggravé pendant le vol
# -----------------------------------------------------------------------------
# 4. Convertir air_time (minutes) en heures
delayed_departures <- delayed_departures |>
  mutate(
    gain = dep_delay_min - arr_delay_min,
    hours = air_time_min / 60
  )

# -----------------------------------------------------------------------------
# 3. Identifier les vols ayant rattrapé plus de 30 minutes pendant le vol
# -----------------------------------------------------------------------------
recovered <- delayed_departures |> filter(gain > 30)
message(sprintf(
  "Vols partis en retard (>= 60 min) ayant rattrapé > 30 min en vol : %d (%.1f%% des départs très retardés)",
  nrow(recovered), 100 * nrow(recovered) / nrow(delayed_departures)
))

# -----------------------------------------------------------------------------
# 5. Calculer gain_per_hour = gain / hours (uniquement pour les vols ayant
#    effectivement rattrapé du retard, hours > 0)
# -----------------------------------------------------------------------------
recovered <- recovered |>
  filter(hours > 0) |>
  mutate(gain_per_hour = gain / hours)

# -----------------------------------------------------------------------------
# 6. Classer les vols selon leur performance de récupération
# -----------------------------------------------------------------------------
ranked_recovery <- recovered |>
  arrange(desc(gain_per_hour)) |>
  select(carrier, flight, tailnum, origin, dest, dep_delay_min, arr_delay_min, gain, hours, gain_per_hour)

# Note de lecture : gain/gain_per_hour mesurent le retard RÉCUPÉRÉ en vol
# (dep_delay - arr_delay), pas le retard final. Un vol en tête de ce
# classement peut donc très bien arriver encore en retard, juste beaucoup
# moins qu'il ne l'était au décollage.
message("Top 10 des vols ayant le mieux rattrapé leur retard (gain/heure) :")
message("(dep_delay_min/arr_delay_min = retards en minutes ; gain = dep_delay - arr_delay = retard récupéré en vol)")
print(head(ranked_recovery, 10))

# Vue par compagnie : qui récupère le mieux le retard au départ ?
by_carrier <- recovered |>
  group_by(carrier) |>
  summarise(
    vols_recuperes = n(),
    gain_moyen = mean(gain),
    gain_par_heure_moyen = mean(gain_per_hour),
    .groups = "drop"
  ) |>
  arrange(desc(gain_par_heure_moyen))

message("\nPerformance de récupération par compagnie :")
print(by_carrier)

# -----------------------------------------------------------------------------
# Visualisation
# -----------------------------------------------------------------------------
p_top <- ranked_recovery |>
  slice_max(gain_per_hour, n = 15) |>
  mutate(label = paste(carrier, flight)) |>
  ggplot(aes(x = reorder(label, gain_per_hour), y = gain_per_hour)) +
  geom_col(fill = "#16A34A") +
  coord_flip() +
  labs(
    title = "Top 15 des vols ayant le mieux rattrapé leur retard",
    subtitle = "Parmi les vols partis avec ≥ 60 min de retard et ayant rattrapé > 30 min en vol",
    x = NULL, y = "Gain de retard par heure de vol (min/h)"
  ) +
  theme_minimal(base_size = 12)

p_carrier <- ggplot(by_carrier, aes(x = reorder(carrier, gain_par_heure_moyen), y = gain_par_heure_moyen)) +
  geom_col(fill = "#2563EB") +
  coord_flip() +
  labs(
    title = "Gain moyen de retard rattrapé par heure de vol, par compagnie",
    x = "Compagnie", y = "Gain moyen (min/h)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "rattrapage_top_vols.png"), p_top, width = 9, height = 6, dpi = 150)
ggsave(file.path(output_dir, "rattrapage_par_compagnie.png"), p_carrier, width = 8, height = 6, dpi = 150)

# -----------------------------------------------------------------------------
# 7. Interprétation
# -----------------------------------------------------------------------------
message("\n--- Interprétation ---")
message(sprintf(
  "Sur %d vols partis avec un retard >= 60 min, %d (%.1f%%) ont rattrapé plus de 30 min en vol.",
  nrow(delayed_departures), nrow(recovered), 100 * nrow(recovered) / nrow(delayed_departures)
))
if (nrow(by_carrier) > 0) {
  best <- by_carrier[1, ]
  message(sprintf(
    "La compagnie la plus efficace pour rattraper le retard en vol est %s (%.1f min récupérées par heure de vol en moyenne).",
    best$carrier, best$gain_par_heure_moyen
  ))
}

message("\nGraphiques exportés dans : ", normalizePath(output_dir))

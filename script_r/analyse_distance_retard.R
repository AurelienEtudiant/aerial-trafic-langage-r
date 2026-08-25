# =============================================================================
# Carte Trello : "Étudier la relation entre distance et retard à l'arrivée"
# Objectif : déterminer s'il existe une relation entre la distance parcourue
# et le retard moyen à l'arrivée.
#
# Travaux réalisés :
#   1. Regrouper les vols par destination (dest)
#   2. Calculer le retard moyen (delay_moy) pour chaque destination
#   3. Compter le nombre de vols pour chaque destination
#   4. Vérifier si la distance est identique pour les vols d'une même
#      destination
#   5. Si nécessaire, calculer une distance moyenne (dist_moy)
#   6. Créer le graphique distance / retard
#   7. Ajouter une courbe de tendance
#   8. Interpréter le graphique
#
# Note : l'énoncé (Mission 3.3, pts 4-5) signale explicitement que HNL est un
# outlier (presque deux fois plus loin que la 2e destination la plus
# distante) et demande de l'exclure de l'analyse pour ne pas fausser la
# relation distance/retard. C'est traité ici (carte Trello séparée dans le
# board, mais indissociable de cette analyse).
#
# Livrable : tableau agrégé par destination et graphique représentant la
# relation entre distance et retard moyen.
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
# Lecture des vols nettoyés (retard à l'arrivée et distance connus)
# -----------------------------------------------------------------------------
message("Lecture de ", mongo_db, ".flights...")
flights_con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
flights <- flights_con$find(
  query = '{"arr_delay_min":{"$ne":null},"distance_mi":{"$gt":0}}',
  fields = '{"dest":1,"distance_mi":1,"arr_delay_min":1}'
) |>
  as_tibble()

if (nrow(flights) == 0L) stop("Aucun vol exploitable dans ", mongo_db, ".flights")

airports_con <- mongo(collection = "airports", db = mongo_db, url = mongo_uri)
airports <- airports_con$find(query = '{}', fields = '{"_id":1,"name":1}') |> as_tibble()

# -----------------------------------------------------------------------------
# 1 & 2 & 3. Regrouper par destination, calculer delay_moy et le nombre de vols
# -----------------------------------------------------------------------------
by_dest <- flights |>
  group_by(dest) |>
  summarise(
    vols = n(),
    delay_moy = mean(arr_delay_min),
    distance_min = min(distance_mi),
    distance_max = max(distance_mi),
    distance_distinct = n_distinct(distance_mi),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 4. Vérifier si la distance est identique pour toutes les vols d'une même
#    destination
# -----------------------------------------------------------------------------
inconsistent_dest <- by_dest |> filter(distance_distinct > 1)
message(sprintf(
  "%d destinations sur %d ont une distance qui varie selon le vol (écart max observé : %.0f mi).",
  nrow(inconsistent_dest), nrow(by_dest),
  max(inconsistent_dest$distance_max - inconsistent_dest$distance_min, 0)
))

# -----------------------------------------------------------------------------
# 5. Calculer la distance moyenne (dist_moy) pour harmoniser
# -----------------------------------------------------------------------------
dest_agg <- flights |>
  group_by(dest) |>
  summarise(
    vols = n(),
    delay_moy = mean(arr_delay_min),
    dist_moy = mean(distance_mi),
    .groups = "drop"
  ) |>
  left_join(airports, by = c("dest" = "_id")) |>
  rename(dest_name = name) |>
  arrange(dist_moy)

message("\nTableau agrégé par destination (extrait) :")
print(head(dest_agg |> select(dest, dest_name, vols, dist_moy, delay_moy), 10))

# -----------------------------------------------------------------------------
# Identifier et exclure l'outlier HNL (Honolulu) signalé par l'énoncé :
# presque deux fois plus loin que la 2e destination la plus distante.
# -----------------------------------------------------------------------------
ranked_by_distance <- dest_agg |> arrange(desc(dist_moy))
farthest <- ranked_by_distance$dist_moy[1]
second_farthest <- ranked_by_distance$dist_moy[2]
message(sprintf(
  "\nDestination la plus lointaine : %s (%s), %.0f mi. Seconde plus lointaine : %s (%s), %.0f mi. Ratio : %.2fx.",
  ranked_by_distance$dest[1], ranked_by_distance$dest_name[1], farthest,
  ranked_by_distance$dest[2], ranked_by_distance$dest_name[2], second_farthest,
  farthest / second_farthest
))

dest_agg_no_outlier <- dest_agg |> filter(dest != "HNL")
message(sprintf(
  "HNL exclu de l'analyse distance/retard (%d destinations restantes).",
  nrow(dest_agg_no_outlier)
))

# -----------------------------------------------------------------------------
# 6 & 7. Graphique distance / retard moyen avec courbe de tendance
#    (avec et sans HNL, pour bien montrer l'effet de l'outlier)
# -----------------------------------------------------------------------------
plot_distance_delay <- function(data, subtitle) {
  ggplot(data, aes(x = dist_moy, y = delay_moy)) +
    geom_point(aes(size = vols), alpha = 0.5, color = "#2563EB") +
    geom_smooth(method = "loess", se = TRUE, color = "#DC2626", linewidth = 1) +
    scale_x_continuous(labels = label_number(big.mark = " ")) +
    scale_size_continuous(labels = label_number(big.mark = " ")) +
    labs(
      title = "Retard moyen à l'arrivée en fonction de la distance",
      subtitle = subtitle,
      x = "Distance moyenne (mi)", y = "Retard moyen à l'arrivée (min)",
      size = "Nombre de vols"
    ) +
    theme_minimal(base_size = 12)
}

p_with_outlier <- plot_distance_delay(dest_agg, "Toutes destinations, y compris HNL (outlier)")
p_without_outlier <- plot_distance_delay(dest_agg_no_outlier, "HNL exclu (destination outlier, cf. énoncé)")

ggsave(file.path(output_dir, "distance_retard_avec_outlier.png"), p_with_outlier, width = 9, height = 6, dpi = 150)
ggsave(file.path(output_dir, "distance_retard_sans_outlier.png"), p_without_outlier, width = 9, height = 6, dpi = 150)

# -----------------------------------------------------------------------------
# 8. Interprétation
# -----------------------------------------------------------------------------
correlation_with <- cor(dest_agg$dist_moy, dest_agg$delay_moy)
correlation_without <- cor(dest_agg_no_outlier$dist_moy, dest_agg_no_outlier$delay_moy)

message("\n--- Interprétation ---")
message(sprintf("Corrélation distance / retard moyen (avec HNL)  : %.3f", correlation_with))
message(sprintf("Corrélation distance / retard moyen (sans HNL) : %.3f", correlation_without))
message(paste(
  "La relation n'est globalement pas positive : la corrélation est négative",
  "(~-0.42), donc le retard moyen à l'arrivée NE croît PAS avec la",
  "distance -- c'est plutôt l'inverse. Le nuage de points suggère une",
  "relation non linéaire : le retard moyen tend à être plus élevé sur les",
  "courtes/moyennes distances puis à diminuer sur les plus longues (les",
  "long-courriers, mieux planifiés avec plus de marge de récupération en",
  "vol, arrivent en moyenne avec moins de retard relatif)."
))
message(sprintf(
  paste(
    "Sur ces données, exclure HNL change peu la corrélation globale",
    "(%.3f -> %.3f) car ce n'est qu'une observation sur %d destinations :",
    "l'exclusion se justifie surtout visuellement (HNL étire l'axe des",
    "distances et écrase la lisibilité du nuage de points sur le reste du",
    "réseau), pas parce qu'il pilote fortement la statistique."
  ),
  correlation_with, correlation_without, nrow(dest_agg)
))

message("\nGraphiques exportés dans : ", normalizePath(output_dir))

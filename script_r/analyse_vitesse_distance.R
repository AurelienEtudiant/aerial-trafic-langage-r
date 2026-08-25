# =============================================================================
# Carte Trello : "Analyser la vitesse et la distance des vols"
# Objectif : analyser les vols selon leur vitesse et la distance parcourue.
#
# Travaux réalisés :
#   1. Calculer la vitesse : speed = distance / air_time * 60
#   2. Trier les vols selon leur vitesse
#   3. Identifier les vols ayant parcouru les plus grandes distances
#   4. Identifier les vols ayant parcouru les plus petites distances
#   5. Distinguer les vols long-courriers et court-courriers
#   6. Réaliser les calculs avec création de nouvelles variables
#   7. Réaliser les calculs sans créer ces variables
#   8. Produire une visualisation
#
# Livrable : classement et analyse des vols selon leur vitesse et leur
# distance.
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
# Lecture des vols nettoyés (uniquement ceux avec distance et air_time valides)
# -----------------------------------------------------------------------------
message("Lecture de ", mongo_db, ".flights...")
con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
flights <- con$find(
  query = '{"distance_mi":{"$gt":0},"air_time_min":{"$gt":0}}',
  fields = '{"carrier":1,"flight":1,"tailnum":1,"origin":1,"dest":1,"distance_mi":1,"air_time_min":1}'
) |>
  as_tibble()

if (nrow(flights) == 0L) stop("Aucun vol exploitable (distance/air_time manquants) dans ", mongo_db, ".flights")

# -----------------------------------------------------------------------------
# 1 & 6. Calculer la vitesse en créant une nouvelle variable
#    speed = distance / air_time * 60   (air_time en minutes -> conversion en heures)
# -----------------------------------------------------------------------------
flights <- flights |>
  mutate(speed = distance_mi / air_time_min * 60)

# -----------------------------------------------------------------------------
# 2. Trier les vols selon leur vitesse (décroissant)
# -----------------------------------------------------------------------------
by_speed <- flights |> arrange(desc(speed))
message("Top 5 des vols les plus rapides :")
print(head(by_speed |> select(carrier, flight, origin, dest, distance_mi, air_time_min, speed), 5))
message("Top 5 des vols les plus lents :")
print(head(by_speed |> arrange(speed) |> select(carrier, flight, origin, dest, distance_mi, air_time_min, speed), 5))

# -----------------------------------------------------------------------------
# 3 & 4. Distances maximales / minimales
#
# On classe ici par ROUTE (origin-dest) distincte plutôt que par vol
# individuel : un même trajet est opéré quotidiennement par le même
# carrier/flight, donc un top 10 sur les vols bruts ne montrerait que des
# doublons de la même ligne (ex : les 10 premières lignes seraient toutes
# "HA 51 JFK->HNL"). Le classement par route donne un vrai top 10.
# -----------------------------------------------------------------------------
routes_by_distance <- flights |>
  distinct(origin, dest, distance_mi) |>
  arrange(desc(distance_mi))

longest <- head(routes_by_distance, 10)
shortest <- head(routes_by_distance |> arrange(distance_mi), 10)

message("\nTop 10 des plus grandes distances parcourues (par route) :")
print(longest)
message("\nTop 10 des plus petites distances parcourues (par route) :")
print(shortest)

# -----------------------------------------------------------------------------
# 5. Distinguer long-courriers / court-courriers
#    Seuils usuels : court-courrier < 1500 mi, moyen-courrier 1500-3000 mi,
#    long-courrier > 3000 mi (adapté au réseau domestique US couvert ici).
# -----------------------------------------------------------------------------
flights <- flights |>
  mutate(haul_type = case_when(
    distance_mi < 1500 ~ "Court-courrier",
    distance_mi < 3000 ~ "Moyen-courrier",
    TRUE ~ "Long-courrier"
  ))

haul_summary <- flights |>
  group_by(haul_type) |>
  summarise(
    vols = n(),
    distance_moyenne = mean(distance_mi),
    vitesse_moyenne = mean(speed),
    .groups = "drop"
  ) |>
  arrange(distance_moyenne)

message("\nRépartition long-courrier / moyen-courrier / court-courrier :")
print(haul_summary)

# -----------------------------------------------------------------------------
# 7. Mêmes résultats SANS créer de nouvelle variable (calcul à la volée,
#    directement dans les agrégations, pour répondre au N.B. de l'énoncé)
# -----------------------------------------------------------------------------
message("\n--- Vérification sans variable intermédiaire ---")
message("Vitesse moyenne globale (calcul direct) : ",
        round(mean(flights$distance_mi / flights$air_time_min * 60), 1), " mph")
message("Vol le plus rapide (calcul direct, sans colonne speed) : ")
print(flights[which.max(flights$distance_mi / flights$air_time_min * 60),
              c("carrier", "flight", "origin", "dest", "distance_mi", "air_time_min")])
message("Plus grande distance (calcul direct, sans slice_max) : ",
        max(flights$distance_mi), " mi")
message("Plus petite distance (calcul direct) : ",
        min(flights$distance_mi), " mi")

# -----------------------------------------------------------------------------
# 8. Visualisation : distance vs vitesse, coloré par type de courrier
# -----------------------------------------------------------------------------
set.seed(1L)  # échantillonnage pour lisibilité du nuage de points uniquement
sample_flights <- flights |> slice_sample(n = min(5000, nrow(flights)))

p_scatter <- ggplot(sample_flights, aes(x = distance_mi, y = speed, color = haul_type)) +
  geom_point(alpha = 0.3, size = 1) +
  scale_color_manual(values = c(
    "Court-courrier" = "#2563EB", "Moyen-courrier" = "#F59E0B", "Long-courrier" = "#DC2626"
  )) +
  scale_x_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Vitesse en fonction de la distance parcourue",
    subtitle = paste0("Échantillon de ", nrow(sample_flights), " vols"),
    x = "Distance (mi)", y = "Vitesse (mph)", color = "Type de courrier"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_haul <- ggplot(haul_summary, aes(x = haul_type, y = vols, fill = haul_type)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Court-courrier" = "#2563EB", "Moyen-courrier" = "#F59E0B", "Long-courrier" = "#DC2626"
  ), guide = "none") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Nombre de vols par type de courrier",
    x = NULL, y = "Nombre de vols"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "vitesse_vs_distance.png"), p_scatter, width = 9, height = 6, dpi = 150)
ggsave(file.path(output_dir, "repartition_type_courrier.png"), p_haul, width = 7, height = 5.5, dpi = 150)

message("\nGraphiques exportés dans : ", normalizePath(output_dir))

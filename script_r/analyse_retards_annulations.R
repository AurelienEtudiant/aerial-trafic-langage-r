# =============================================================================
# ANALYSE DES RETARDS ET DES ANNULATIONS
#
# Objectif :
# Identifier où et chez quelles compagnies les problèmes de retard
# sont les plus importants et analyser les annulations.
# =============================================================================


# =============================================================================
# 1. CHARGEMENT DES PACKAGES
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(mongolite)
  library(lubridate)
})


# =============================================================================
# 2. CONNEXION A MONGODB
# =============================================================================

mongo_uri <- Sys.getenv("MONGODB_URI")
mongo_db <- Sys.getenv(
  "MONGO_CLEAN_DB",
  unset = "nyc_flights_cleaned"
)

output_dir <- Sys.getenv(
  "OUTPUT_DIR",
  unset = "output"
)

if (!nzchar(mongo_uri)) {
  stop("MONGODB_URI est absente.")
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# =============================================================================
# 3. LECTURE DES DONNEES
# =============================================================================

message("Lecture des vols depuis MongoDB...")

con <- mongo(
  collection = "flights",
  db = mongo_db,
  url = mongo_uri
)

flights <- con$find(
  query = '{}',
  fields = '{}'
) |>
  as_tibble()

if (nrow(flights) == 0) {
  stop("Aucun vol trouvé dans MongoDB.")
}

message(
  "Nombre total de vols : ",
  format(nrow(flights), big.mark = " ")
)


# =============================================================================
# 4. PREPARATION DES DONNEES
# =============================================================================

flights <- flights |>
  mutate(
    dep_delay_min = as.numeric(dep_delay_min),
    arr_delay_min = as.numeric(arr_delay_min),
    dep_time = as.numeric(dep_time),
    arr_time = as.numeric(arr_time),
    sched_dep_time = as.numeric(sched_dep_time)
  )


# =============================================================================
# PARTIE A : ANALYSE DES RETARDS
# =============================================================================


# =============================================================================
# 5. CLASSER LES DESTINATIONS PAR RETARD
# =============================================================================

message("\n==============================")
message("DESTINATIONS PAR RETARD")
message("==============================")

classement_destinations <- flights |>
  group_by(dest) |>
  summarise(
    nb_vols = n(),
    retard_moyen_arrivee =
      mean(arr_delay_min, na.rm = TRUE),
    retard_moyen_depart =
      mean(dep_delay_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(retard_moyen_arrivee))

print(classement_destinations)


# Top 10
message("\nTop 10 destinations avec le plus de retard :")

print(
  classement_destinations |>
    slice_head(n = 10)
)


# =============================================================================
# 6. CLASSER LES COMPAGNIES PAR RETARD
# =============================================================================

message("\n==============================")
message("COMPAGNIES PAR RETARD")
message("==============================")

classement_compagnies <- flights |>
  group_by(carrier) |>
  summarise(
    nb_vols = n(),
    retard_moyen_arrivee =
      mean(arr_delay_min, na.rm = TRUE),
    retard_moyen_depart =
      mean(dep_delay_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(retard_moyen_arrivee))

print(classement_compagnies)


message("\nTop compagnies avec le plus de retard :")

print(
  classement_compagnies |>
    slice_head(n = 10)
)


# =============================================================================
# 7. RESUME STATISTIQUE DE arr_delay
# =============================================================================

message("\n==============================")
message("STATISTIQUES arr_delay")
message("==============================")

resume_arr_delay <- flights |>
  summarise(
    nombre = sum(!is.na(arr_delay_min)),
    moyenne = mean(arr_delay_min, na.rm = TRUE),
    mediane = median(arr_delay_min, na.rm = TRUE),
    ecart_type = sd(arr_delay_min, na.rm = TRUE),
    minimum = min(arr_delay_min, na.rm = TRUE),
    Q1 = quantile(
      arr_delay_min,
      0.25,
      na.rm = TRUE
    ),
    Q3 = quantile(
      arr_delay_min,
      0.75,
      na.rm = TRUE
    ),
    maximum = max(arr_delay_min, na.rm = TRUE)
  )

print(resume_arr_delay)


# =============================================================================
# 8. RESUME STATISTIQUE DE dep_delay
# =============================================================================

message("\n==============================")
message("STATISTIQUES dep_delay")
message("==============================")

resume_dep_delay <- flights |>
  summarise(
    nombre = sum(!is.na(dep_delay_min)),
    moyenne = mean(dep_delay_min, na.rm = TRUE),
    mediane = median(dep_delay_min, na.rm = TRUE),
    ecart_type = sd(dep_delay_min, na.rm = TRUE),
    minimum = min(dep_delay_min, na.rm = TRUE),
    Q1 = quantile(
      dep_delay_min,
      0.25,
      na.rm = TRUE
    ),
    Q3 = quantile(
      dep_delay_min,
      0.75,
      na.rm = TRUE
    ),
    maximum = max(dep_delay_min, na.rm = TRUE)
  )

print(resume_dep_delay)


# =============================================================================
# 9. DISTRIBUTION DES RETARDS A L'ARRIVEE
# =============================================================================

p_arr_delay <- ggplot(
  flights,
  aes(x = arr_delay_min)
) +
  geom_histogram(
    bins = 60,
    na.rm = TRUE
  ) +
  coord_cartesian(
    xlim = c(-50, 200)
  ) +
  labs(
    title = "Distribution des retards à l'arrivée",
    x = "Retard à l'arrivée (minutes)",
    y = "Nombre de vols"
  ) +
  theme_minimal()

print(p_arr_delay)

ggsave(
  file.path(
    output_dir,
    "distribution_retard_arrivee.png"
  ),
  p_arr_delay,
  width = 9,
  height = 6,
  dpi = 150
)


# =============================================================================
# 10. DISTRIBUTION DES RETARDS AU DEPART
# =============================================================================

p_dep_delay <- ggplot(
  flights,
  aes(x = dep_delay_min)
) +
  geom_histogram(
    bins = 60,
    na.rm = TRUE
  ) +
  coord_cartesian(
    xlim = c(-50, 200)
  ) +
  labs(
    title = "Distribution des retards au départ",
    x = "Retard au départ (minutes)",
    y = "Nombre de vols"
  ) +
  theme_minimal()

print(p_dep_delay)

ggsave(
  file.path(
    output_dir,
    "distribution_retard_depart.png"
  ),
  p_dep_delay,
  width = 9,
  height = 6,
  dpi = 150
)


# =============================================================================
# 11. RETARD MOYEN PAR AEROPORT
# =============================================================================

message("\n==============================")
message("RETARD MOYEN PAR AEROPORT")
message("==============================")

retard_aeroport <- flights |>
  group_by(origin) |>
  summarise(
    nb_vols = n(),
    retard_moyen_arrivee =
      mean(arr_delay_min, na.rm = TRUE),
    retard_moyen_depart =
      mean(dep_delay_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(retard_moyen_arrivee)

print(retard_aeroport)


# =============================================================================
# 12. AEROPORT AVEC LE RETARD MOYEN LE PLUS FAIBLE
# =============================================================================

message("\nAéroport avec le retard moyen à l'arrivée le plus faible :")

aeroport_min <- retard_aeroport |>
  slice_min(
    retard_moyen_arrivee,
    n = 1,
    with_ties = FALSE
  )

print(aeroport_min)


# =============================================================================
# 13. ANALYSE SELON L'HEURE DE DEPART
# =============================================================================

flights <- flights |>
  mutate(
    heure_depart = floor(sched_dep_time / 100)
  )

retard_par_heure <- flights |>
  group_by(heure_depart) |>
  summarise(
    nb_vols = n(),
    retard_moyen_depart =
      mean(dep_delay_min, na.rm = TRUE),
    retard_moyen_arrivee =
      mean(arr_delay_min, na.rm = TRUE),
    .groups = "drop"
  )

message("\n==============================")
message("RETARD SELON L'HEURE")
message("==============================")

print(retard_par_heure)


# =============================================================================
# 14. GRAPHIQUE RETARD / HEURE
# =============================================================================

p_heure <- ggplot(
  retard_par_heure,
  aes(
    x = heure_depart,
    y = retard_moyen_depart
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Retard moyen selon l'heure de départ",
    x = "Heure de départ prévue",
    y = "Retard moyen au départ (minutes)"
  ) +
  theme_minimal()

print(p_heure)

ggsave(
  file.path(
    output_dir,
    "retard_selon_heure.png"
  ),
  p_heure,
  width = 9,
  height = 6,
  dpi = 150
)


# =============================================================================
# PARTIE B : ANNULATIONS
# =============================================================================


# =============================================================================
# 15. IDENTIFIER LES VOLS ANNULES
# =============================================================================

flights <- flights |>
  mutate(
    cancelled =
      is.na(dep_time) &
      is.na(arr_time)
  )


# =============================================================================
# 16. NOMBRE TOTAL DE VOLS ANNULES
# =============================================================================

nb_annulations <- sum(flights$cancelled)

message("\n==============================")
message("ANNULATIONS")
message("==============================")

message(
  "Nombre total de vols annulés : ",
  format(nb_annulations, big.mark = " ")
)


# =============================================================================
# 17. TAUX D'ANNULATION
# =============================================================================

taux_annulation <-
  mean(flights$cancelled) * 100

message(
  "Taux d'annulation : ",
  round(taux_annulation, 2),
  "%"
)


# =============================================================================
# 18. ANALYSER LES ANNULATIONS PAR DESTINATION
# =============================================================================

annulations_dest <- flights |>
  group_by(dest) |>
  summarise(
    nb_vols = n(),
    nb_annulations = sum(cancelled),
    taux_annulation =
      mean(cancelled) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(nb_annulations))

message("\n==============================")
message("ANNULATIONS PAR DESTINATION")
message("==============================")

print(annulations_dest)


# =============================================================================
# 19. ANALYSER LES ANNULATIONS PAR COMPAGNIE
# =============================================================================

annulations_compagnie <- flights |>
  group_by(carrier) |>
  summarise(
    nb_vols = n(),
    nb_annulations = sum(cancelled),
    taux_annulation =
      mean(cancelled) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(nb_annulations))

message("\n==============================")
message("ANNULATIONS PAR COMPAGNIE")
message("==============================")

print(annulations_compagnie)


# =============================================================================
# 20. EVOLUTION DES ANNULATIONS DANS LE TEMPS
# =============================================================================

annulations_temps <- flights |>
  group_by(year, month) |>
  summarise(
    nb_vols = n(),
    nb_annulations = sum(cancelled),
    taux_annulation =
      mean(cancelled) * 100,
    .groups = "drop"
  ) |>
  mutate(
    date = make_date(year, month, 1)
  )

message("\n==============================")
message("ANNULATIONS DANS LE TEMPS")
message("==============================")

print(annulations_temps)


# =============================================================================
# 21. GRAPHIQUE DES ANNULATIONS DANS LE TEMPS
# =============================================================================

p_annulations_temps <- ggplot(
  annulations_temps,
  aes(
    x = date,
    y = taux_annulation
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolution du taux d'annulation",
    x = "Date",
    y = "Taux d'annulation (%)"
  ) +
  theme_minimal()

print(p_annulations_temps)

ggsave(
  file.path(
    output_dir,
    "evolution_annulations.png"
  ),
  p_annulations_temps,
  width = 9,
  height = 6,
  dpi = 150
)


# =============================================================================
# 22. GRAPHIQUE DES ANNULATIONS PAR COMPAGNIE
# =============================================================================

top_compagnies_annulations <-
  annulations_compagnie |>
  slice_head(n = 10)

p_annulations_compagnie <- ggplot(
  top_compagnies_annulations,
  aes(
    x = reorder(
      carrier,
      nb_annulations
    ),
    y = nb_annulations
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 des compagnies par nombre d'annulations",
    x = "Compagnie",
    y = "Nombre d'annulations"
  ) +
  theme_minimal()

print(p_annulations_compagnie)

ggsave(
  file.path(
    output_dir,
    "annulations_par_compagnie.png"
  ),
  p_annulations_compagnie,
  width = 9,
  height = 6,
  dpi = 150
)


# =============================================================================
# 23. GRAPHIQUE DES ANNULATIONS PAR DESTINATION
# =============================================================================

top_dest_annulations <-
  annulations_dest |>
  slice_head(n = 15)

p_annulations_dest <- ggplot(
  top_dest_annulations,
  aes(
    x = reorder(
      dest,
      nb_annulations
    ),
    y = nb_annulations
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 des destinations par nombre d'annulations",
    x = "Destination",
    y = "Nombre d'annulations"
  ) +
  theme_minimal()

print(p_annulations_dest)

ggsave(
  file.path(
    output_dir,
    "annulations_par_destination.png"
  ),
  p_annulations_dest,
  width = 9,
  height = 7,
  dpi = 150
)


# =============================================================================
# 24. TRI dep_delay DECROISSANT AVEC NA EN PREMIER
# =============================================================================

tri_dep_delay <- flights |>
  arrange(
    desc(is.na(dep_delay_min)),
    desc(dep_delay_min)
  )

message("\n==============================")
message("TRI dep_delay")
message("==============================")

print(
  tri_dep_delay |>
    select(
      carrier,
      flight,
      origin,
      dest,
      dep_delay_min,
      dep_time
    ) |>
    head(20)
)


# =============================================================================
# 25. TRI dep_delay DECROISSANT
#     ET dep_time CROISSANT AVEC NA EN PREMIER
# =============================================================================

tri_dep_delay_heure <- flights |>
  arrange(
    desc(is.na(dep_delay_min)),
    desc(dep_delay_min),
    dep_time
  )

message("\n==============================")
message("TRI dep_delay + dep_time")
message("==============================")

print(
  tri_dep_delay_heure |>
    select(
      carrier,
      flight,
      origin,
      dest,
      dep_delay_min,
      dep_time
    ) |>
    head(20)
)


# =============================================================================
# 26. CORRELATION ENTRE HEURE ET RETARD
# =============================================================================

correlation <- cor(
  flights$heure_depart,
  flights$dep_delay_min,
  use = "complete.obs"
)

message(
  "\nCorrélation entre l'heure de départ et le retard : ",
  round(correlation, 3)
)


# =============================================================================
# 27. INTERPRETATION AUTOMATIQUE
# =============================================================================

message("\n==============================")
message("INTERPRETATION")
message("==============================")

message(
  "Le retard moyen au départ est de ",
  round(
    mean(
      flights$dep_delay_min,
      na.rm = TRUE
    ),
    2
  ),
  " minutes."
)

message(
  "Le retard moyen à l'arrivée est de ",
  round(
    mean(
      flights$arr_delay_min,
      na.rm = TRUE
    ),
    2
  ),
  " minutes."
)

if (nrow(classement_destinations) > 0) {

  best_dest <-
    classement_destinations |>
    slice_max(
      retard_moyen_arrivee,
      n = 1,
      with_ties = FALSE
    )

  message(
    "La destination avec le retard moyen à l'arrivée le plus élevé est ",
    best_dest$dest,
    " avec ",
    round(
      best_dest$retard_moyen_arrivee,
      2
    ),
    " minutes."
  )
}

if (nrow(classement_compagnies) > 0) {

  best_carrier <-
    classement_compagnies |>
    slice_max(
      retard_moyen_arrivee,
      n = 1,
      with_ties = FALSE
    )

  message(
    "La compagnie avec le retard moyen à l'arrivée le plus élevé est ",
    best_carrier$carrier,
    " avec ",
    round(
      best_carrier$retard_moyen_arrivee,
      2
    ),
    " minutes."
  )
}

message(
  "L'aéroport avec le retard moyen à l'arrivée le plus faible est ",
  aeroport_min$origin,
  " avec ",
  round(
    aeroport_min$retard_moyen_arrivee,
    2
  ),
  " minutes."
)

message(
  "Au total, ",
  format(nb_annulations, big.mark = " "),
  " vols sont annulés, soit ",
  round(taux_annulation, 2),
  "% des vols."
)

message(
  "\nLes graphiques ont été enregistrés dans : ",
  normalizePath(output_dir)
)
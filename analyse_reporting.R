

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)


# 0. CHARGEMENT DES DONNEES NETTOYEES


if (!exists("flights_clean") || !exists("airlines_clean") ||
    !exists("planes_clean") || !exists("weather_clean") ||
    !exists("airports_clean")) {
  flights_clean  <- read_csv("data_clean/flights_clean.csv", show_col_types = FALSE)
  airports_clean <- read_csv("data_clean/airports_clean.csv", show_col_types = FALSE)
  airlines_clean <- read_csv("data_clean/airlines_clean.csv", show_col_types = FALSE)
  planes_clean   <- read_csv("data_clean/planes_clean.csv",   show_col_types = FALSE)
  weather_clean  <- read_csv("data_clean/weather_clean.csv",  show_col_types = FALSE)
}



# SECTION 3.1 - PICS DE TRAFIC AEROPORTUAIRE


## --- Q2 : viz par facette - trafic mensuel des 3 aéroports + moyenne ---

trafic_mensuel <- flights_clean %>%
  mutate(mois = month(time_hour, label = TRUE, abbr = TRUE)) %>%
  group_by(origin, mois) %>%
  summarise(
    nb_vols_total = n(),
    nb_jours      = n_distinct(as.Date(time_hour)),
    vols_par_jour = nb_vols_total / nb_jours,
    .groups = "drop"
  )

moyenne_par_aeroport <- trafic_mensuel %>%
  group_by(origin) %>%
  summarise(moyenne_vols_jour = mean(vols_par_jour), .groups = "drop")

graph_trafic_mensuel <- ggplot(trafic_mensuel, aes(x = mois, y = vols_par_jour, group = origin)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(data = moyenne_par_aeroport,
             aes(yintercept = moyenne_vols_jour),
             color = "firebrick", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ origin, ncol = 1, scales = "free_y") +
  labs(
    title = "Trafic mensuel moyen par jour, par aéroport d'origine",
    subtitle = "Ligne pointillée rouge = moyenne annuelle de l'aéroport",
    x = "Mois", y = "Nombre moyen de vols / jour"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

graph_trafic_mensuel


## --- Q3 : taux d'accroissement mensuel ---

trafic_mensuel_evol <- trafic_mensuel %>%
  arrange(origin, mois) %>%
  group_by(origin) %>%
  mutate(
    taux_accroissement = (vols_par_jour - lag(vols_par_jour)) / lag(vols_par_jour) * 100
  ) %>%
  ungroup()



pics_trafic <- trafic_mensuel %>%
  left_join(moyenne_par_aeroport, by = "origin") %>%
  mutate(ecart_pct = round((vols_par_jour - moyenne_vols_jour) / moyenne_vols_jour * 100, 1)) %>%
  group_by(origin) %>%
  slice_max(vols_par_jour, n = 1) %>%
  ungroup() %>%
  select(origin, mois, vols_par_jour, ecart_pct)

pics_trafic


## --- Q4 : top 3 aéroports origine / destination ---



top3_origine <- flights_clean %>%
  count(origin, name = "nb_vols") %>%
  arrange(desc(nb_vols)) %>%
  slice_head(n = 3)

top3_destination <- flights_clean %>%
  count(dest, name = "nb_vols") %>%
  left_join(airports_clean, by = c("dest" = "faa")) %>%
  select(dest, name, nb_vols) %>%
  arrange(desc(nb_vols)) %>%
  slice_head(n = 3)

top3_origine
top3_destination


## --- Q5 : filtres sur dates spécifiques ---



vols_1er_janvier <- flights_clean %>%
  filter(as.Date(time_hour) == as.Date("2013-01-01"))

vols_par_jour_2013 <- flights_clean %>%
  mutate(date = as.Date(time_hour)) %>%
  count(date, name = "nb_vols") %>%
  arrange(date)

vols_nov_dec <- flights_clean %>%
  filter(month(time_hour) %in% c(11, 12))

jours_speciaux <- as.Date(c("2013-12-25", "2013-01-01", "2013-07-04", "2013-11-29"))

vols_jours_speciaux <- flights_clean %>%
  filter(as.Date(time_hour) %in% jours_speciaux)

vols_ete <- flights_clean %>%
  filter(month(time_hour) %in% c(7, 8, 9))

vols_nuit <- flights_clean %>%
  filter(hour(dep_time_datetime) >= 0 & hour(dep_time_datetime) <= 6)


## --- Q6 : weekend vs jours ouvrés + jours spéciaux vs moyenne + viz ---



trafic_weekend <- flights_clean %>%
  mutate(
    date = as.Date(time_hour),
    est_weekend = wday(time_hour, week_start = 1) %in% c(6, 7)
  ) %>%
  group_by(est_weekend) %>%
  summarise(
    nb_vols_total = n(),
    nb_jours = n_distinct(date),
    vols_par_jour = nb_vols_total / nb_jours,
    .groups = "drop"
  )

moyenne_annuelle <- flights_clean %>%
  mutate(date = as.Date(time_hour)) %>%
  count(date) %>%
  summarise(moyenne = mean(n)) %>%
  pull(moyenne)

ecart_jours_speciaux <- vols_jours_speciaux %>%
  mutate(date = as.Date(time_hour)) %>%
  count(date, name = "nb_vols") %>%
  mutate(
    moyenne_annuelle = moyenne_annuelle,
    ecart_pct = round((nb_vols - moyenne_annuelle) / moyenne_annuelle * 100, 1)
  )

vols_par_jour_annotes <- flights_clean %>%
  mutate(date = as.Date(time_hour)) %>%
  count(date, name = "nb_vols") %>%
  mutate(
    type_jour = case_when(
      date %in% jours_speciaux ~ "Jour spécial",
      wday(date, week_start = 1) %in% c(6, 7) ~ "Weekend",
      TRUE ~ "Jour ouvré"
    )
  )

graph_trafic_journalier <- ggplot(vols_par_jour_annotes, aes(x = date, y = nb_vols)) +
  geom_line(color = "grey60", linewidth = 0.4) +
  geom_point(aes(color = type_jour, size = type_jour), alpha = 0.7) +
  geom_hline(yintercept = moyenne_annuelle, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Jour ouvré" = "steelblue",
                                  "Weekend" = "orange",
                                  "Jour spécial" = "firebrick")) +
  scale_size_manual(values = c("Jour ouvré" = 0.8, "Weekend" = 0.8, "Jour spécial" = 3)) +
  labs(
    title = "Evolution du trafic journalier - vols au départ de NYC (2013)",
    subtitle = "Ligne pointillée = moyenne annuelle · Points rouges = jours spéciaux",
    x = NULL, y = "Nombre de vols",
    color = "Type de jour", size = "Type de jour"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

graph_trafic_journalier


# SECTION 3.2 - RETARD A L'ARRIVEE ET/OU AU DEPART

## --- Q1 : vols les plus retardés (arrivée / départ / les deux) ---

top10_retard_arrivee <- flights_clean %>%
  filter(!is.na(arr_delay)) %>%
  arrange(desc(arr_delay)) %>%
  slice_head(n = 10) %>%
  select(carrier, flight, origin, dest, dep_delay, arr_delay, time_hour)

top10_retard_depart <- flights_clean %>%
  filter(!is.na(dep_delay)) %>%
  arrange(desc(dep_delay)) %>%
  slice_head(n = 10) %>%
  select(carrier, flight, origin, dest, dep_delay, arr_delay, time_hour)

top10_retard_les_deux <- flights_clean %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  mutate(retard_total = dep_delay + arr_delay) %>%
  arrange(desc(retard_total)) %>%
  slice_head(n = 10) %>%
  select(carrier, flight, origin, dest, dep_delay, arr_delay, retard_total, time_hour)




## --- Q2 : retard moyen global + retard moyen journalier ---



retard_moyen_global <- flights_clean %>%
  summarise(retard_moyen_dep = mean(dep_delay, na.rm = TRUE))

retard_moyen_journalier <- flights_clean %>%
  mutate(date = as.Date(time_hour)) %>%
  group_by(date) %>%
  summarise(retard_moyen_dep = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(retard_moyen_dep))

graph_retard_journalier <- ggplot(retard_moyen_journalier, aes(x = date, y = retard_moyen_dep)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = retard_moyen_global$retard_moyen_dep,
             linetype = "dashed", color = "firebrick") +
  labs(
    title = "Retard moyen au départ, par jour (2013)",
    subtitle = "Ligne pointillée = retard moyen annuel",
    x = NULL, y = "Retard moyen au départ (min)"
  ) +
  theme_minimal()

graph_retard_journalier


## --- Q3 : vols +2h retard arrivée malgré départ à l'heure / vols ponctuels / avances ---

                             

vols_rattrapage_negatif <- flights_clean %>%
  filter(dep_delay <= 0, arr_delay > 120)

vols_ponctuels <- flights_clean %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  filter(dep_delay <= 120, arr_delay <= 120)

vols_depart_avance <- flights_clean %>%
  filter(dep_delay < 0)

vols_arrivee_avancee <- flights_clean %>%
  filter(arr_delay < 0)


## --- Q4 : vols partis avec >= 1h de retard, ayant rattrapé > 30 min ---
## --- Q5 : gain par heure ---
## --- Q6 : vitesse (speed) ---



vols_rattrapage_partiel <- flights_clean %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  filter(dep_delay >= 60) %>%
  mutate(gain = dep_delay - arr_delay) %>%
  filter(gain > 30)

vols_avec_gain <- flights_clean %>%
  filter(!is.na(dep_delay), !is.na(arr_delay), !is.na(air_time)) %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  ) %>%
  filter(gain > 0)

vols_avec_vitesse <- flights_clean %>%
  filter(!is.na(air_time), air_time > 0) %>%
  mutate(speed = distance / air_time * 60) %>%
  arrange(desc(speed))


## --- Q7 : résumé statistique de arr_delay et dep_delay + viz ---



resume_stat_retards_detail <- flights_clean %>%
  summarise(
    dep_delay_moy = mean(dep_delay, na.rm = TRUE),
    dep_delay_med = median(dep_delay, na.rm = TRUE),
    dep_delay_sd  = sd(dep_delay, na.rm = TRUE),
    arr_delay_moy = mean(arr_delay, na.rm = TRUE),
    arr_delay_med = median(arr_delay, na.rm = TRUE),
    arr_delay_sd  = sd(arr_delay, na.rm = TRUE)
  )

graph_distribution_retards <- flights_clean %>%
  select(dep_delay, arr_delay) %>%
  tidyr::pivot_longer(everything(), names_to = "type", values_to = "retard") %>%
  filter(!is.na(retard)) %>%
  ggplot(aes(x = type, y = retard)) +
  geom_boxplot(fill = "steelblue", outlier.alpha = 0.1) +
  coord_cartesian(ylim = c(-50, 100)) +
  labs(
    title = "Distribution des retards au départ et à l'arrivée",
    subtitle = "Zoomé sur [-50, 100] min pour la lisibilité (outliers non affichés mais conservés)",
    x = NULL, y = "Retard (minutes)"
  ) +
  theme_minimal()

graph_distribution_retards



## --- Q8 : aéroport avec le retard moyen le plus faible ---



retard_par_aeroport <- flights_clean %>%
  filter(!is.na(dep_delay)) %>%
  group_by(origin) %>%
  summarise(retard_moyen = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
  arrange(retard_moyen)

retard_par_aeroport


## --- Q9 : relation entre heure de décollage et retard ---



retard_par_heure <- flights_clean %>%
  filter(!is.na(dep_delay)) %>%
  mutate(heure_dep = hour(dep_time_datetime)) %>%
  group_by(heure_dep) %>%
  summarise(
    retard_moyen = mean(dep_delay, na.rm = TRUE),
    nb_vols = n(),
    .groups = "drop"
  )

retard_par_heure


graph_retard_par_heure <- retard_par_heure %>%
  filter(nb_vols >= 100) %>%
  ggplot(aes(x = heure_dep, y = retard_moyen)) +
  geom_col(aes(fill = nb_vols)) +
  scale_fill_gradient(low = "lightblue", high = "steelblue", name = "Nb de vols") +
  labs(
    title = "Retard moyen au départ, selon l'heure de décollage",
    subtitle = "Heures avec < 100 vols exclues (moyenne non fiable, ex: 0h-3h)",
    x = "Heure de décollage", y = "Retard moyen (min)"
  ) +
  theme_minimal()

graph_retard_par_heure


# SECTION 3.3 - RETARD VS DISTANCE


retard_distance_par_dest <- flights_clean %>%
  filter(!is.na(arr_delay), !is.na(distance)) %>%
  group_by(dest) %>%
  summarise(
    delay_moy    = mean(arr_delay, na.rm = TRUE),
    dist_moy     = mean(distance, na.rm = TRUE),
    nb_distances_distinctes = n_distinct(distance),
    count        = n(),
    .groups = "drop"
  )


## --- Etape 4 : identifier l'outlier HNL ---


retard_distance_par_dest %>%
  arrange(desc(dist_moy)) %>%
  head(5)


## --- Etape 5 : exclure HNL et tracer le graphique ---



retard_distance_sans_outlier <- retard_distance_par_dest %>%
  filter(dest != "HNL")

graph_retard_distance <- ggplot(retard_distance_sans_outlier,
                                  aes(x = dist_moy, y = delay_moy, size = count)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(aes(weight = count), method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Relation entre la distance et le retard moyen à l'arrivée",
    subtitle = "HNL exclu (outlier) · Taille des points = nombre de vols",
    x = "Distance moyenne (miles)", y = "Retard moyen à l'arrivée (min)",
    size = "Nb de vols"
  ) +
  theme_minimal()

graph_retard_distance





# SECTION 1.2 - CARTOGRAPHIE DES TRAJECTOIRES AERIENNES

library(maps)



routes_geo <- flights_clean %>%
  count(origin, dest, name = "nb_vols") %>%
  left_join(airports_clean %>% select(faa, lat, lon), by = c("origin" = "faa")) %>%
  rename(origin_lat = lat, origin_lon = lon) %>%
  left_join(airports_clean %>% select(faa, lat, lon), by = c("dest" = "faa")) %>%
  rename(dest_lat = lat, dest_lon = lon) %>%
  filter(!is.na(origin_lat), !is.na(dest_lat))
 

us_map <- map_data("state")

## --- Carte complète : toutes les routes ---

graph_carte_usa <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "white") +
  geom_segment(data = routes_geo,
               aes(x = origin_lon, y = origin_lat, xend = dest_lon, yend = dest_lat,
                   linewidth = nb_vols, alpha = nb_vols),
               color = "steelblue",
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_point(data = routes_geo %>% distinct(dest, dest_lon, dest_lat),
             aes(x = dest_lon, y = dest_lat), color = "firebrick", size = 1.2) +
  scale_linewidth(range = c(0.1, 2), guide = "none") +
  scale_alpha(range = c(0.1, 0.8), guide = "none") +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "Trajectoires aériennes au départ de NYC (2013)",
       subtitle = "Epaisseur/opacité proportionnelle au nombre de vols")

graph_carte_usa

## --- Top 100 routes les plus empruntées ---

top100_routes <- routes_geo %>%
  arrange(desc(nb_vols)) %>%
  slice_head(n = 100)

graph_carte_top100 <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "white") +
  geom_segment(data = top100_routes,
               aes(x = origin_lon, y = origin_lat, xend = dest_lon, yend = dest_lat,
                   linewidth = nb_vols),
               color = "firebrick", alpha = 0.6) +
  geom_point(data = top100_routes %>% distinct(dest, dest_lon, dest_lat),
             aes(x = dest_lon, y = dest_lat), color = "black", size = 1.5) +
  scale_linewidth(range = c(0.3, 2.5), guide = "none") +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "Top 100 des routes les plus empruntées au départ de NYC")

graph_carte_top100


## --- Top 5 destinations les plus fréquentées ---

top5_destinations <- flights_clean %>%
  count(dest, name = "nb_vols") %>%
  left_join(airports_clean, by = c("dest" = "faa")) %>%
  arrange(desc(nb_vols)) %>%
  slice_head(n = 5)

top5_destinations %>% select(dest, name, nb_vols)



# SECTION 3.4 - VOLS ANNULES / DONNEES MANQUANTES


## --- Q1 : proportion de NA par colonne ---


proportion_na <- flights_clean %>%
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 2))) %>%
  tidyr::pivot_longer(everything(), names_to = "colonne", values_to = "pct_na") %>%
  filter(pct_na > 0) %>%
  arrange(desc(pct_na))

proportion_na


## --- Q2 : vols annulés, affinés par destination et par compagnie ---


nb_vols_annules <- flights_clean %>% filter(cancelled) %>% nrow()

annulations_par_dest <- flights_clean %>%
  filter(cancelled) %>%
  count(dest, name = "nb_annulations") %>%
  arrange(desc(nb_annulations))

annulations_par_carrier <- flights_clean %>%
  filter(cancelled) %>%
  count(carrier, name = "nb_annulations") %>%
  left_join(airlines_clean, by = "carrier") %>%
  arrange(desc(nb_annulations))


graph_annulations_carrier <- ggplot(annulations_par_carrier,
                                      aes(x = reorder(name, nb_annulations), y = nb_annulations)) +
  geom_col(fill = "firebrick", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Nombre de vols annulés par compagnie",
    x = NULL, y = "Nombre d'annulations"
  ) +
  theme_minimal()

graph_annulations_carrier



## --- Q3 : tri par dep_delay décroissant, NA en premier ---

flights_tri_na_premier <- flights_clean %>%
  arrange(desc(is.na(dep_delay)), desc(dep_delay))


## --- Q4 : tri par dep_delay décroissant ET dep_time croissant, NA en premier ---

flights_tri_double <- flights_clean %>%
  arrange(desc(is.na(dep_delay)), desc(dep_delay), is.na(dep_time_datetime), dep_time_datetime)


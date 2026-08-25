# =============================================================================
# Carte Trello : "Comparer le trafic : semaine, week-end et jours spéciaux"
# Objectif : déterminer si le niveau de trafic varie selon le type de journée.
#
# Travaux réalisés :
#   1. Créer la variable jour de la semaine
#   2. Identifier les jours ouvrés
#   3. Identifier les jours de fin de semaine
#   4. Calculer le trafic moyen en jours ouvrés
#   5. Calculer le trafic moyen en fin de semaine
#   6. Calculer la moyenne annuelle
#   7. Comparer les jours spéciaux à la moyenne annuelle
#   8. Créer le graphique comparatif
#   9. Interpréter les résultats
#
# Livrable : visualisation comparative et interprétation des différences
# observées.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(mongolite)
  library(scales)
  library(stringr)
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
  query = '{}',
  fields = '{"date":1,"year":1,"month":1,"day":1,"origin":1,"origin_matched":1}'
) |>
  as_tibble()

if (nrow(flights) == 0L) stop("Collection vide : ", mongo_db, ".flights")

# -----------------------------------------------------------------------------
# 1. Créer la variable jour de la semaine
# -----------------------------------------------------------------------------
# Libellés fixés en dur (plutôt que wday(..., locale = "fr_FR")) : l'image
# Docker rocker/r-ver n'a pas nécessairement la locale fr_FR installée, ce
# qui rendrait ce script fragile selon l'environnement d'exécution.
weekday_names_fr <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")

flights <- flights |>
  mutate(
    date = as_date(date),
    weekday_num = wday(date, week_start = 1),          # 1 = lundi ... 7 = dimanche
    weekday_label = factor(weekday_names_fr[weekday_num], levels = weekday_names_fr)
  )

# -----------------------------------------------------------------------------
# 2 & 3. Identifier jours ouvrés / jours de fin de semaine
# -----------------------------------------------------------------------------
flights <- flights |>
  mutate(day_type = if_else(weekday_num >= 6, "Week-end", "Jour ouvré"))

# -----------------------------------------------------------------------------
# Jours spéciaux demandés dans l'énoncé (Mission 3.1 Q5) : Noël, Jour de l'an,
# Independence Day, Thanksgiving.
#
# ATTENTION : l'énoncé fixe Thanksgiving au "29/11", ce qui n'est vrai que
# pour certaines années (Thanksgiving US = 4e jeudi de novembre, une date
# mobile). Les données de ce projet portent sur l'année 2021 (vérifié par
# requête sur flights.date), où le 4e jeudi de novembre tombe le 25/11, et le
# 29/11/2021 est un lundi (le fameux "Cyber Monday", pas Thanksgiving).
# On calcule donc la vraie date de Thanksgiving dynamiquement à partir des
# années présentes dans les données, et on garde en plus le 29/11 littéral de
# l'énoncé à titre de comparaison, pour que rien ne soit silencieusement
# corrigé sans que ce soit visible dans les résultats.
# -----------------------------------------------------------------------------
nth_weekday_of_month <- function(year, month, weekday_iso, n) {
  # weekday_iso : 1 = lundi ... 7 = dimanche (convention ISO utilisée plus haut)
  first_of_month <- make_date(year, month, 1)
  first_weekday <- wday(first_of_month, week_start = 1)
  offset <- (weekday_iso - first_weekday) %% 7
  first_of_month + days(offset + 7 * (n - 1))
}

years_in_data <- flights |> pull(year) |> unique() |> sort()
thanksgiving_dates <- nth_weekday_of_month(years_in_data, 11L, weekday_iso = 4L, n = 4L)  # 4e jeudi

special_days <- tibble::tibble(
  special_label = c("Jour de l'an", "Independence Day", "Noël"),
  month = c(1L, 7L, 12L),
  day = c(1L, 4L, 25L)
) |>
  tidyr::crossing(year = years_in_data) |>
  bind_rows(
    tibble::tibble(
      special_label = "Thanksgiving (vrai, 4e jeudi de nov.)",
      year = years_in_data,
      month = month(thanksgiving_dates),
      day = day(thanksgiving_dates)
    )
  ) |>
  bind_rows(
    tibble::tibble(
      special_label = "29/11 (date de l'énoncé)",
      year = years_in_data,
      month = 11L,
      day = 29L
    )
  )

flights <- flights |>
  left_join(special_days, by = c("year", "month", "day")) |>
  mutate(is_special = !is.na(special_label))

# -----------------------------------------------------------------------------
# 4 & 5. Trafic moyen (nombre de vols par jour) en jours ouvrés / week-end
# -----------------------------------------------------------------------------
daily_counts <- flights |>
  count(date, weekday_label, day_type, is_special, special_label, name = "flights")

avg_by_day_type <- daily_counts |>
  group_by(day_type) |>
  summarise(mean_daily_flights = mean(flights), .groups = "drop")

message("Trafic quotidien moyen par type de jour :")
print(avg_by_day_type)

# -----------------------------------------------------------------------------
# 6. Moyenne annuelle (tous jours confondus)
# -----------------------------------------------------------------------------
annual_mean <- mean(daily_counts$flights)
message(sprintf("Moyenne annuelle de vols/jour : %.1f", annual_mean))

# -----------------------------------------------------------------------------
# 7. Comparer les jours spéciaux à la moyenne annuelle
# -----------------------------------------------------------------------------
special_comparison <- daily_counts |>
  filter(is_special) |>
  mutate(
    diff_vs_annual = flights - annual_mean,
    diff_pct_vs_annual = 100 * (flights / annual_mean - 1)
  ) |>
  select(date, special_label, weekday_label, flights, diff_vs_annual, diff_pct_vs_annual) |>
  arrange(date)

message("Comparaison jours spéciaux vs moyenne annuelle :")
print(special_comparison)

# -----------------------------------------------------------------------------
# 8. Graphique comparatif : trafic moyen par jour de semaine, jours spéciaux
#    surlignés
# -----------------------------------------------------------------------------
by_weekday <- daily_counts |>
  group_by(weekday_label, day_type) |>
  summarise(mean_flights = mean(flights), .groups = "drop")

p_weekday <- ggplot(by_weekday, aes(x = weekday_label, y = mean_flights, fill = day_type)) +
  geom_col() +
  geom_hline(yintercept = annual_mean, linetype = "dashed", color = "#475569") +
  annotate("label", x = 1, y = annual_mean, label = "Moyenne annuelle",
           vjust = -0.4, hjust = 0, size = 3.2, color = "#475569",
           fill = "white", alpha = 0.85) +
  scale_fill_manual(values = c("Jour ouvré" = "#2563EB", "Week-end" = "#E11D48")) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Trafic moyen par jour de la semaine",
    x = NULL, y = "Vols / jour (moyenne)", fill = "Type de jour"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_special <- ggplot(special_comparison, aes(x = reorder(special_label, date), y = flights, fill = diff_vs_annual >= 0)) +
  geom_col() +
  geom_hline(yintercept = annual_mean, linetype = "dashed", color = "#475569") +
  scale_fill_manual(values = c(`TRUE` = "#16A34A", `FALSE` = "#DC2626"), guide = "none") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Trafic des jours spéciaux vs moyenne annuelle",
    subtitle = sprintf("Ligne pointillée = moyenne annuelle (%.0f vols/jour)", annual_mean),
    x = NULL, y = "Vols ce jour-là"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "jours_speciaux_par_weekday.png"), p_weekday, width = 9, height = 5.5, dpi = 150)
ggsave(file.path(output_dir, "jours_speciaux_vs_moyenne.png"), p_special, width = 8, height = 5.5, dpi = 150)

# -----------------------------------------------------------------------------
# 9. Interprétation (imprimée en sortie de script)
# -----------------------------------------------------------------------------
weekend_vs_weekday <- avg_by_day_type |>
  tidyr::pivot_wider(names_from = day_type, values_from = mean_daily_flights)

message("\n--- Interprétation ---")
message(sprintf(
  "Jours ouvrés : %.1f vols/jour en moyenne ; Week-end : %.1f vols/jour en moyenne (%.1f%% de moins).",
  weekend_vs_weekday$`Jour ouvré`, weekend_vs_weekday$`Week-end`,
  100 * (1 - weekend_vs_weekday$`Week-end` / weekend_vs_weekday$`Jour ouvré`)
))
for (i in seq_len(nrow(special_comparison))) {
  row <- special_comparison[i, ]
  message(sprintf(
    "%s (%s) : %d vols, soit %+.1f%% par rapport à la moyenne annuelle.",
    row$special_label, format(row$date, "%d/%m/%Y"), row$flights, row$diff_pct_vs_annual
  ))
}

message("\nGraphiques exportés dans : ", normalizePath(output_dir))

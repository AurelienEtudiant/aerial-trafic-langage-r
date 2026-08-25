suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(mongolite)
  library(stringr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
mongo_uri <- Sys.getenv("MONGODB_URI")
source_database <- Sys.getenv("MONGO_SOURCE_DB", unset = "nyc_flights")

target_database <- Sys.getenv("MONGO_CLEAN_DB", unset = "nyc_flights_cleaned")

if (!nzchar(mongo_uri)) {
  stop("MONGODB_URI absente. Définissez-la dans .env ou passez-la en premier argument.")
}

source_collections <- c("airlines", "airports", "planes", "weather", "flights")

connection <- function(collection, database) {
  mongo(collection = collection, db = database, url = mongo_uri)
}

normalise_na <- function(data) {
  data |>
    mutate(across(where(is.character), ~na_if(str_trim(.x), ""))) |>
    mutate(across(
      where(is.character),
      ~replace(.x, .x %in% c("NA", "NaN", "None", "null"), NA_character_)
    ))
}

as_integer_safe <- function(x) suppressWarnings(as.integer(as.numeric(x)))
as_numeric_safe <- function(x) suppressWarnings(as.numeric(x))

hhmm_valid <- function(x) {
  x <- as_integer_safe(x)
  hours <- x %/% 100L
  minutes <- x %% 100L
  is.na(x) | (
    hours >= 0L & hours <= 24L &
      minutes >= 0L & minutes <= 59L &
      !(hours == 24L & minutes != 0L)
  )
}

iqr_flag <- function(x, multiplier = 3) {
  x <- as_numeric_safe(x)
  if (all(is.na(x))) return(rep(FALSE, length(x)))
  limits <- quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  spread <- limits[[2]] - limits[[1]]
  !is.na(x) & (
    x < limits[[1]] - multiplier * spread |
      x > limits[[2]] + multiplier * spread
  )
}

read_collection <- function(name) {
  message("Lecture de ", source_database, ".", name, "...")
  data <- connection(name, source_database)$find(
    query = '{}',
    fields = '{}'
  ) |>
    as_tibble(.name_repair = "minimal")

  if (!"_id" %in% names(data) && "X_id" %in% names(data)) {
    data <- data |> rename(`_id` = X_id)
  }
  if (!"_id" %in% names(data) && "id" %in% names(data)) {
    data <- data |> rename(`_id` = id)
  }
  if (!"_id" %in% names(data)) {
    stop(
      "Champ _id absent de ", name,
      ". Champs disponibles : ", paste(names(data), collapse = ", ")
    )
  }
  data
}

write_collection <- function(name, data, chunk_size = 5000L) {
  target <- connection(name, target_database)
  if (target$count() > 0L) target$drop()

  if (nrow(data) == 0L) {
    message(target_database, ".", name, " : collection vide")
    return(invisible(name))
  }

  starts <- seq.int(1L, nrow(data), by = chunk_size)
  for (start in starts) {
    end <- min(start + chunk_size - 1L, nrow(data))
    target$insert(data[start:end, , drop = FALSE])
  }

  message(target_database, ".", name, " : ", target$count(), " documents écrits")
  invisible(name)
}

missing_report <- function(data, collection_name) {
  tibble(
    collection = collection_name,
    field = names(data),
    missing_count = vapply(data, function(x) sum(is.na(x)), integer(1)),
    missing_pct = round(100 * missing_count / max(nrow(data), 1L), 3)
  )
}

airlines <- read_collection("airlines") |>
  normalise_na() |>
  mutate(
    `_id` = str_to_upper(`_id`),
    name = str_squish(name),
    valid_carrier = !is.na(`_id`) & str_detect(`_id`, "^[A-Z0-9]{2}$")
  ) |>
  distinct(`_id`, .keep_all = TRUE)

airports <- read_collection("airports") |>
  normalise_na() |>
  mutate(
    `_id` = str_to_upper(`_id`),
    name = str_squish(name),
    alt = as_integer_safe(alt),
    tz = as_integer_safe(tz),
    dst = str_to_upper(dst),
    valid_faa = !is.na(`_id`) & str_detect(`_id`, "^[A-Z0-9]{3,4}$"),
    valid_dst = is.na(dst) | dst %in% c("A", "U", "N")
  ) |>
  distinct(`_id`, .keep_all = TRUE)

planes <- read_collection("planes") |>
  normalise_na() |>
  mutate(
    `_id` = str_to_upper(`_id`),
    year = as_integer_safe(year),
    engines = as_integer_safe(engines),
    seats = as_integer_safe(seats),
    speed = as_numeric_safe(speed),
    valid_tailnum = !is.na(`_id`) & str_detect(`_id`, "^N[0-9A-Z]{1,5}$"),
    valid_engines = is.na(engines) | engines > 0L,
    valid_seats = is.na(seats) | seats > 0L
  ) |>
  distinct(`_id`, .keep_all = TRUE)

weather <- read_collection("weather") |>
  normalise_na() |>
  mutate(
    origin = str_to_upper(origin),
    across(any_of(c("year", "month", "day", "hour")), as_integer_safe),
    across(
      any_of(c("temp_f", "dewpoint_f", "humidity_pct", "wind_dir_deg",
               "wind_speed_mph", "wind_gust_mph", "precip_in",
               "pressure_mb", "visibility_mi")),
      as_numeric_safe
    ),
    valid_origin = !is.na(origin) & str_detect(origin, "^[A-Z0-9]{3,4}$"),
    valid_humidity = is.na(humidity_pct) | between(humidity_pct, 0, 100),
    valid_wind_direction = is.na(wind_dir_deg) | between(wind_dir_deg, 0, 360),
    valid_precipitation = is.na(precip_in) | precip_in >= 0,
    valid_visibility = is.na(visibility_mi) | visibility_mi >= 0,
    airport_matched = origin %in% airports$`_id`
  ) |>
  distinct(`_id`, .keep_all = TRUE)

flights_raw <- read_collection("flights")
if (!"tailnum_raw" %in% names(flights_raw)) flights_raw$tailnum_raw <- NA_character_

flights <- flights_raw |>
  normalise_na() |>
  mutate(
    across(
      any_of(c("year", "month", "day", "hour", "minute", "dep_time",
               "arr_time", "sched_dep_time", "sched_arr_time",
               "dep_delay_min", "arr_delay_min", "flight",
               "air_time_min", "distance_mi")),
      as_integer_safe
    ),
    carrier = str_to_upper(carrier),
    origin = str_to_upper(origin),
    dest = str_to_upper(dest),
    source_tailnum = coalesce(tailnum_raw, tailnum),
    source_tailnum = str_to_upper(source_tailnum),
    tailnum = if_else(
      !is.na(source_tailnum) & str_detect(source_tailnum, "^N[0-9A-Z]{1,5}$"),
      source_tailnum,
      NA_character_
    ),
    tailnum_raw = if_else(source_tailnum != tailnum | is.na(tailnum), source_tailnum, NA_character_),
    invalid_tailnum = !is.na(source_tailnum) & is.na(tailnum),
    dep_time_invalid = !hhmm_valid(dep_time),
    arr_time_invalid = !hhmm_valid(arr_time),
    sched_dep_time_invalid = !hhmm_valid(sched_dep_time),
    sched_arr_time_invalid = !hhmm_valid(sched_arr_time),
    dep_delay_outlier = iqr_flag(dep_delay_min),
    arr_delay_outlier = iqr_flag(arr_delay_min),
    distance_invalid = is.na(distance_mi) | distance_mi <= 0L,
    air_time_invalid = !is.na(air_time_min) & air_time_min <= 0L
  ) |>
  mutate(
    dep_time = replace(dep_time, dep_time_invalid, NA_integer_),
    arr_time = replace(arr_time, arr_time_invalid, NA_integer_),
    sched_dep_time = replace(sched_dep_time, sched_dep_time_invalid, NA_integer_),
    sched_arr_time = replace(sched_arr_time, sched_arr_time_invalid, NA_integer_),
    plane_matched = !is.na(tailnum) & tailnum %in% planes$`_id`,
    origin_matched = origin %in% airports$`_id`,
    destination_matched = dest %in% airports$`_id`,
    carrier_matched = carrier %in% airlines$`_id`
  ) |>
  select(-source_tailnum) |>
  distinct(`_id`, .keep_all = TRUE)

datasets <- list(
  airlines = airlines,
  airports = airports,
  planes = planes,
  weather = weather,
  flights = flights
)

quality_summary <- bind_rows(
  tibble(collection="airlines", control="invalid_carrier", count=sum(!airlines$valid_carrier, na.rm=TRUE)),
  tibble(collection="airports", control="invalid_faa", count=sum(!airports$valid_faa, na.rm=TRUE)),
  tibble(collection="planes", control="invalid_tailnum", count=sum(!planes$valid_tailnum, na.rm=TRUE)),
  tibble(collection="weather", control="invalid_humidity", count=sum(!weather$valid_humidity, na.rm=TRUE)),
  tibble(collection="weather", control="unmatched_airport", count=sum(!weather$airport_matched, na.rm=TRUE)),
  tibble(collection="flights", control="invalid_tailnum", count=sum(flights$invalid_tailnum, na.rm=TRUE)),
  tibble(collection="flights", control="unmatched_plane", count=sum(!flights$plane_matched, na.rm=TRUE)),
  tibble(collection="flights", control="unmatched_origin", count=sum(!flights$origin_matched, na.rm=TRUE)),
  tibble(collection="flights", control="unmatched_destination", count=sum(!flights$destination_matched, na.rm=TRUE)),
  tibble(collection="flights", control="unmatched_carrier", count=sum(!flights$carrier_matched, na.rm=TRUE)),
  tibble(collection="flights", control="departure_delay_outlier_3iqr", count=sum(flights$dep_delay_outlier, na.rm=TRUE)),
  tibble(collection="flights", control="arrival_delay_outlier_3iqr", count=sum(flights$arr_delay_outlier, na.rm=TRUE))
)

missing_summary <- bind_rows(Map(missing_report, datasets, names(datasets)))

for (name in names(datasets)) {
  write_collection(name, datasets[[name]])
}

report <- connection("data_quality_reports", target_database)
if (report$count() > 0L) report$drop()
generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
report$insert(tibble(
  report_type = "metadata",
  generated_at = generated_at,
  collection = names(datasets),
  documents = vapply(datasets, nrow, integer(1)),
  source_database = source_database,
  target_database = target_database
))
report$insert(quality_summary |> mutate(report_type = "quality", generated_at = generated_at))
report$insert(missing_summary |> mutate(report_type = "missing", generated_at = generated_at))

print(quality_summary, n = Inf)
message("Nettoyage terminé : ", source_database, " -> ", target_database)
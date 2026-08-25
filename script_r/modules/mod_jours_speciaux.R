# =============================================================================
# Module Shiny : "Comparer le trafic : semaine, week-end et jours spéciaux"
# Version interactive de script_r/analyse_jours_speciaux.R, intégrée à app.R.
# La logique de calcul (jour de semaine, jours spéciaux, comparaison à la
# moyenne annuelle) est la même que le script CLI, adaptée en reactive().
# =============================================================================

jours_speciaux_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "Comparaison du trafic entre jours ouvrés, week-ends et jours spéciaux (Jour de l'an, Independence Day, Thanksgiving, Noël).",
      class = "text-muted"
    ),
    plotlyOutput(ns("weekday_plot"), height = "420px"),
    h4("Jours spéciaux vs moyenne annuelle"),
    plotlyOutput(ns("special_plot"), height = "380px"),
    DTOutput(ns("special_table"))
  )
}

jours_speciaux_server <- function(id, mongo_uri, mongo_db) {
  moduleServer(id, function(input, output, session) {

    weekday_names_fr <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")

    nth_weekday_of_month <- function(year, month, weekday_iso, n) {
      first_of_month <- make_date(year, month, 1)
      first_weekday <- wday(first_of_month, week_start = 1)
      offset <- (weekday_iso - first_weekday) %% 7
      first_of_month + days(offset + 7 * (n - 1))
    }

    flights_by_day <- reactive({
      con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
      data <- con$find(
        query = '{}',
        fields = '{"date":1,"year":1,"month":1,"day":1}'
      ) |>
        as_tibble()
      validate(need(nrow(data) > 0, "Collection flights vide."))

      data <- data |>
        mutate(
          date = as_date(date),
          weekday_num = wday(date, week_start = 1),
          weekday_label = factor(weekday_names_fr[weekday_num], levels = weekday_names_fr),
          day_type = if_else(weekday_num >= 6, "Week-end", "Jour ouvré")
        )

      years_in_data <- data |> pull(year) |> unique() |> sort()
      thanksgiving_dates <- nth_weekday_of_month(years_in_data, 11L, weekday_iso = 4L, n = 4L)

      special_days <- tibble::tibble(
        special_label = c("Jour de l'an", "Independence Day", "Noël"),
        month = c(1L, 7L, 12L),
        day = c(1L, 4L, 25L)
      ) |>
        tidyr::crossing(year = years_in_data) |>
        bind_rows(tibble::tibble(
          special_label = "Thanksgiving",
          year = years_in_data,
          month = month(thanksgiving_dates),
          day = day(thanksgiving_dates)
        ))

      data |> left_join(special_days, by = c("year", "month", "day")) |>
        mutate(is_special = !is.na(special_label))
    })

    daily_counts <- reactive({
      flights_by_day() |>
        count(date, weekday_label, day_type, is_special, special_label, name = "flights")
    })

    annual_mean <- reactive(mean(daily_counts()$flights))

    by_weekday <- reactive({
      daily_counts() |>
        group_by(weekday_label, day_type) |>
        summarise(mean_flights = mean(flights), .groups = "drop")
    })

    special_comparison <- reactive({
      daily_counts() |>
        filter(is_special) |>
        mutate(
          diff_vs_annual = flights - annual_mean(),
          diff_pct_vs_annual = 100 * (flights / annual_mean() - 1)
        ) |>
        select(date, special_label, weekday_label, flights, diff_vs_annual, diff_pct_vs_annual) |>
        arrange(date)
    })

    output$weekday_plot <- renderPlotly({
      df <- by_weekday()
      p <- ggplot(df, aes(x = weekday_label, y = mean_flights, fill = day_type)) +
        geom_col() +
        geom_hline(yintercept = annual_mean(), linetype = "dashed", color = "#475569") +
        scale_fill_manual(values = c("Jour ouvré" = "#2563EB", "Week-end" = "#E11D48")) +
        scale_y_continuous(labels = label_number(big.mark = " ")) +
        labs(title = "Trafic moyen par jour de la semaine", x = NULL, y = "Vols / jour (moyenne)", fill = "Type de jour") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
      ggplotly(p, tooltip = c("x", "y", "fill"))
    })

    output$special_plot <- renderPlotly({
      df <- special_comparison()
      req(nrow(df) > 0)
      p <- ggplot(df, aes(x = reorder(special_label, date), y = flights, fill = diff_vs_annual >= 0)) +
        geom_col() +
        geom_hline(yintercept = annual_mean(), linetype = "dashed", color = "#475569") +
        scale_fill_manual(values = c(`TRUE` = "#16A34A", `FALSE` = "#DC2626"), guide = "none") +
        scale_y_continuous(labels = label_number(big.mark = " ")) +
        labs(
          title = "Trafic des jours spéciaux vs moyenne annuelle",
          x = NULL, y = "Vols ce jour-là"
        ) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$special_table <- renderDT({
      special_comparison() |>
        transmute(
          Date = format(date, "%d/%m/%Y"),
          `Jour spécial` = special_label,
          `Jour de semaine` = weekday_label,
          Vols = flights,
          `Écart vs moyenne` = round(diff_vs_annual, 1),
          `Écart (%)` = round(diff_pct_vs_annual, 1)
        ) |>
        datatable(rownames = FALSE, options = list(pageLength = 6, dom = "tip"))
    })
  })
}

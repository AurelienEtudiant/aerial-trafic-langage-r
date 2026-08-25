# =============================================================================
# Module Shiny : "Analyser le rattrapage des retards en vol"
# Version interactive de script_r/analyse_rattrapage_retards.R, intégrée à app.R.
#
# ATTENTION signe du gain : gain = dep_delay - arr_delay (gain > 0 = retard
# rattrapé en vol). L'énoncé écrit littéralement l'inverse
# (arr_delay - dep_delay), ce qui contredit son intention métier ("rattraper"
# = arriver moins en retard qu'au départ). Voir analyse_rattrapage_retards.R
# pour le détail de cette correction.
# =============================================================================

rattrapage_retards_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "Vols partis avec au moins 60 minutes de retard ayant rattrapé plus de 30 minutes pendant le vol.",
      class = "text-muted"
    ),
    div(
      class = "metric-grid",
      div(class = "metric-card", div(class = "metric-title", "Départs très retardés (≥ 60 min)"), div(class = "metric-value", textOutput(ns("kpi_delayed"), inline = TRUE))),
      div(class = "metric-card", div(class = "metric-title", "Ont rattrapé > 30 min en vol"), div(class = "metric-value", textOutput(ns("kpi_recovered"), inline = TRUE))),
      div(class = "metric-card", div(class = "metric-title", "Meilleure compagnie"), div(class = "metric-value", textOutput(ns("kpi_best_carrier"), inline = TRUE)))
    ),
    plotlyOutput(ns("carrier_plot"), height = "400px"),
    h4("Top 15 des vols ayant le mieux rattrapé leur retard"),
    DTOutput(ns("top_table"))
  )
}

rattrapage_retards_server <- function(id, mongo_uri, mongo_db) {
  moduleServer(id, function(input, output, session) {

    recovered_flights <- reactive({
      con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
      data <- con$find(
        query = '{"dep_delay_min":{"$ne":null},"arr_delay_min":{"$ne":null},"air_time_min":{"$gt":0}}',
        fields = '{"carrier":1,"flight":1,"tailnum":1,"origin":1,"dest":1,"dep_delay_min":1,"arr_delay_min":1,"air_time_min":1}'
      ) |>
        as_tibble()
      validate(need(nrow(data) > 0, "Aucun vol exploitable (retards/air_time manquants)."))

      data |>
        filter(dep_delay_min >= 60) |>
        mutate(
          gain = dep_delay_min - arr_delay_min,
          hours = air_time_min / 60
        ) |>
        filter(gain > 30, hours > 0) |>
        mutate(gain_per_hour = gain / hours)
    })

    delayed_count <- reactive({
      con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
      con$count(query = '{"dep_delay_min":{"$gte":60}}')
    })

    by_carrier <- reactive({
      recovered_flights() |>
        group_by(carrier) |>
        summarise(
          vols_recuperes = n(),
          gain_moyen = mean(gain),
          gain_par_heure_moyen = mean(gain_per_hour),
          .groups = "drop"
        ) |>
        arrange(desc(gain_par_heure_moyen))
    })

    output$kpi_delayed <- renderText(format(delayed_count(), big.mark = " "))
    output$kpi_recovered <- renderText({
      n <- nrow(recovered_flights())
      pct <- 100 * n / delayed_count()
      sprintf("%s (%.1f%%)", format(n, big.mark = " "), pct)
    })
    output$kpi_best_carrier <- renderText({
      df <- by_carrier()
      if (nrow(df) == 0L) return("ND")
      sprintf("%s (%.1f min/h)", df$carrier[1], df$gain_par_heure_moyen[1])
    })

    output$carrier_plot <- renderPlotly({
      df <- by_carrier()
      req(nrow(df) > 0)
      p <- ggplot(df, aes(x = reorder(carrier, gain_par_heure_moyen), y = gain_par_heure_moyen)) +
        geom_col(fill = "#2563EB") +
        coord_flip() +
        labs(
          title = "Gain moyen de retard rattrapé par heure de vol, par compagnie",
          x = "Compagnie", y = "Gain moyen (min/h)"
        ) +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$top_table <- renderDT({
      recovered_flights() |>
        arrange(desc(gain_per_hour)) |>
        slice_head(n = 15) |>
        transmute(
          Compagnie = carrier,
          Vol = flight,
          Immatriculation = tailnum,
          Origine = origin,
          Destination = dest,
          `Retard départ (min)` = dep_delay_min,
          `Retard arrivée (min)` = arr_delay_min,
          `Retard rattrapé (min)` = gain,
          `Rattrapage (min/h)` = round(gain_per_hour, 1)
        ) |>
        datatable(rownames = FALSE, options = list(pageLength = 10, dom = "tip"))
    })
  })
}

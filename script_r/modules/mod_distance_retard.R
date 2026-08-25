# =============================================================================
# Module Shiny : "Étudier la relation entre distance et retard à l'arrivée"
# Version interactive de script_r/analyse_distance_retard.R, intégrée à app.R.
#
# HNL (Honolulu) est signalé par l'énoncé comme destination outlier
# (~2x plus loin que la 2e destination la plus distante) : un switch permet
# de l'inclure ou de l'exclure pour comparer visuellement son effet.
# =============================================================================

distance_retard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("exclude_hnl"), "Exclure HNL (Honolulu, destination outlier)", value = TRUE),
    plotlyOutput(ns("scatter_plot"), height = "480px"),
    textOutput(ns("correlation_note")),
    h4("Tableau agrégé par destination"),
    DTOutput(ns("dest_table"))
  )
}

distance_retard_server <- function(id, mongo_uri, mongo_db) {
  moduleServer(id, function(input, output, session) {

    dest_agg <- reactive({
      flights_con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
      flights <- flights_con$find(
        query = '{"arr_delay_min":{"$ne":null},"distance_mi":{"$gt":0}}',
        fields = '{"dest":1,"distance_mi":1,"arr_delay_min":1}'
      ) |>
        as_tibble()
      validate(need(nrow(flights) > 0, "Aucun vol exploitable (retard/distance manquants)."))

      airports_con <- mongo(collection = "airports", db = mongo_db, url = mongo_uri)
      airports <- airports_con$find(query = '{}', fields = '{"_id":1,"name":1}') |> as_tibble()

      flights |>
        group_by(dest) |>
        summarise(
          vols = n(),
          delay_moy = mean(arr_delay_min),
          dist_moy = mean(distance_mi),
          .groups = "drop"
        ) |>
        left_join(airports, by = c("dest" = "_id")) |>
        rename(dest_name = name)
    })

    dest_agg_filtered <- reactive({
      df <- dest_agg()
      if (isTRUE(input$exclude_hnl)) df <- df |> filter(dest != "HNL")
      df
    })

    correlation <- reactive({
      df <- dest_agg_filtered()
      if (nrow(df) < 3) return(NA_real_)
      cor(df$dist_moy, df$delay_moy)
    })

    output$scatter_plot <- renderPlotly({
      df <- dest_agg_filtered()
      req(nrow(df) > 0)
      subtitle <- if (isTRUE(input$exclude_hnl)) "HNL exclu (destination outlier)" else "Toutes destinations, y compris HNL"
      p <- ggplot(df, aes(x = dist_moy, y = delay_moy, text = dest_name)) +
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
      ggplotly(p, tooltip = c("text", "x", "y", "size"))
    })

    output$correlation_note <- renderText({
      r <- correlation()
      if (is.na(r)) return("")
      sprintf(
        "Corrélation distance / retard moyen : %.3f (une valeur négative signifie que le retard moyen NE croît PAS avec la distance).",
        r
      )
    })

    output$dest_table <- renderDT({
      dest_agg_filtered() |>
        arrange(desc(delay_moy)) |>
        transmute(
          Destination = dest,
          Nom = dest_name,
          Vols = vols,
          `Distance moy. (mi)` = round(dist_moy, 0),
          `Retard moy. arrivée (min)` = round(delay_moy, 1)
        ) |>
        datatable(rownames = FALSE, filter = "top", options = list(pageLength = 10, dom = "tip"))
    })
  })
}

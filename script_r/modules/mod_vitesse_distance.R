# =============================================================================
# Module Shiny : "Analyser la vitesse et la distance des vols"
# Version interactive de script_r/analyse_vitesse_distance.R, intégrée à app.R.
# =============================================================================

vitesse_distance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "Vitesse (distance / air_time) et distance parcourue par vol, avec distinction court/moyen/long-courrier.",
      class = "text-muted"
    ),
    plotlyOutput(ns("scatter_plot"), height = "450px"),
    fluidRow(
      column(6, plotlyOutput(ns("haul_plot"), height = "350px")),
      column(6, DTOutput(ns("routes_table")))
    )
  )
}

vitesse_distance_server <- function(id, mongo_uri, mongo_db) {
  moduleServer(id, function(input, output, session) {

    flights_speed <- reactive({
      con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
      data <- con$find(
        query = '{"distance_mi":{"$gt":0},"air_time_min":{"$gt":0}}',
        fields = '{"carrier":1,"flight":1,"origin":1,"dest":1,"distance_mi":1,"air_time_min":1}'
      ) |>
        as_tibble()
      validate(need(nrow(data) > 0, "Aucun vol exploitable (distance/air_time manquants)."))

      data |>
        mutate(
          speed = distance_mi / air_time_min * 60,
          haul_type = case_when(
            distance_mi < 1500 ~ "Court-courrier",
            distance_mi < 3000 ~ "Moyen-courrier",
            TRUE ~ "Long-courrier"
          )
        )
    })

    haul_summary <- reactive({
      flights_speed() |>
        group_by(haul_type) |>
        summarise(
          vols = n(),
          distance_moyenne = mean(distance_mi),
          vitesse_moyenne = mean(speed),
          .groups = "drop"
        ) |>
        arrange(distance_moyenne)
    })

    routes_by_distance <- reactive({
      flights_speed() |>
        distinct(origin, dest, distance_mi) |>
        arrange(desc(distance_mi))
    })

    output$scatter_plot <- renderPlotly({
      set.seed(1L)
      df <- flights_speed() |> slice_sample(n = min(5000, nrow(flights_speed())))
      p <- ggplot(df, aes(x = distance_mi, y = speed, color = haul_type)) +
        geom_point(alpha = 0.3, size = 1) +
        scale_color_manual(values = c(
          "Court-courrier" = "#2563EB", "Moyen-courrier" = "#F59E0B", "Long-courrier" = "#DC2626"
        )) +
        scale_x_continuous(labels = label_number(big.mark = " ")) +
        labs(
          title = "Vitesse en fonction de la distance parcourue",
          x = "Distance (mi)", y = "Vitesse (mph)", color = "Type de courrier"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
      ggplotly(p, tooltip = c("x", "y", "colour"))
    })

    output$haul_plot <- renderPlotly({
      df <- haul_summary()
      p <- ggplot(df, aes(x = haul_type, y = vols, fill = haul_type)) +
        geom_col() +
        scale_fill_manual(values = c(
          "Court-courrier" = "#2563EB", "Moyen-courrier" = "#F59E0B", "Long-courrier" = "#DC2626"
        ), guide = "none") +
        scale_y_continuous(labels = label_number(big.mark = " ")) +
        labs(title = "Nombre de vols par type de courrier", x = NULL, y = "Nombre de vols") +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y"))
    })

    output$routes_table <- renderDT({
      bind_rows(
        head(routes_by_distance(), 10) |> mutate(Classement = "Plus longues"),
        head(routes_by_distance() |> arrange(distance_mi), 10) |> mutate(Classement = "Plus courtes")
      ) |>
        transmute(Classement, Origine = origin, Destination = dest, `Distance (mi)` = distance_mi) |>
        datatable(rownames = FALSE, filter = "top", options = list(pageLength = 10, dom = "tip"))
    })
  })
}

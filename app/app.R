suppressPackageStartupMessages({
  library(DT)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(mongolite)
  library(plotly)
  library(scales)
  library(shiny)
  library(stringr)
  library(tidyr)
})

# Modules d'analyse (un fichier par carte Trello, voir script_r/analyse_*.R
# pour les versions CLI équivalentes avec impressions commentées).
# - Dans le conteneur shiny_traffic : copiés à /script_r/modules (racine).
# - En local (docker compose --profile analysis / lancement direct) :
#   app/ et script_r/ sont deux dossiers frères à la racine du repo.
candidate_dirs <- c(
  "/script_r/modules",
  file.path(dirname(getwd()), "script_r", "modules"),
  file.path("script_r", "modules")
)
modules_dir <- candidate_dirs[which(dir.exists(candidate_dirs))[1]]
if (is.na(modules_dir)) stop("Introuvable : dossier des modules Shiny (script_r/modules).")

# En local, exposer les graphiques générés dans output/. Dans le conteneur,
# Docker les copie directement dans /app/www/output, déjà servi par Shiny.
output_candidates <- c(
  "/app/www/output",
  file.path(dirname(getwd()), "output"),
  file.path("output")
)
static_output_dir <- output_candidates[which(dir.exists(output_candidates))[1]]
if (!is.na(static_output_dir) && normalizePath(static_output_dir) != normalizePath(file.path(getwd(), "www", "output"), mustWork = FALSE)) {
  addResourcePath("output", static_output_dir)
}

source(file.path(modules_dir, "mod_jours_speciaux.R"))
source(file.path(modules_dir, "mod_analyse_retards.R"), encoding = "UTF-8")
source(file.path(modules_dir, "mod_rattrapage_retards.R"))
source(file.path(modules_dir, "mod_vitesse_distance.R"))
source(file.path(modules_dir, "mod_distance_retard.R"))

colors <- c(EWR = "#2563EB", JFK = "#E11D48", LGA = "#059669")
mongo_uri <- Sys.getenv("MONGODB_URI")
mongo_db <- Sys.getenv("MONGO_CLEAN_DB", unset = "nyc_flights_cleaned")

if (!nzchar(mongo_uri)) stop("MONGODB_URI est absente")

fetch_monthly <- function() {
  con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
  pipeline <- '[
    {"$match":{"origin":{"$in":["EWR","JFK","LGA"]}}},
    {"$group":{
      "_id":{"origin":"$origin","year":"$year","month":"$month"},
      "flights":{"$sum":1},
      "observed_days_values":{"$addToSet":"$day"}
    }},
    {"$project":{
      "_id":0,
      "origin":"$_id.origin",
      "year":"$_id.year",
      "month":"$_id.month",
      "flights":1,
      "observed_days":{"$size":"$observed_days_values"}
    }},
    {"$sort":{"origin":1,"year":1,"month":1}}
  ]'
  data <- con$aggregate(pipeline)
  if (nrow(data) == 0L) stop(paste0("Collection vide : ", mongo_db, ".flights"))
  data |>
    mutate(
      origin = str_to_upper(origin),
      year = as.integer(year),
      month = as.integer(month),
      flights = as.integer(flights),
      observed_days = as.integer(observed_days),
      month_date = make_date(year, month, 1),
      calendar_days = days_in_month(month_date),
      coverage_pct = 100 * observed_days / calendar_days,
      flights_per_observed_day = flights / observed_days
    )
}

calculate_metrics <- function(observed, threshold) {
  first <- floor_date(min(observed$month_date), "year")
  last <- ceiling_date(max(observed$month_date), "year") - months(1)
  grid <- expand_grid(
    origin = c("EWR", "JFK", "LGA"),
    month_date = seq.Date(first, last, by = "month")
  )
  result <- grid |>
    left_join(observed, by = c("origin", "month_date")) |>
    mutate(
      calendar_days = days_in_month(month_date),
      complete_month = coalesce(coverage_pct >= threshold, FALSE),
      status = case_when(
        is.na(flights) ~ "Absent",
        complete_month ~ "Complet",
        TRUE ~ "Partiel"
      )
    ) |>
    arrange(origin, month_date)

  averages <- result |>
    filter(complete_month) |>
    group_by(origin) |>
    summarise(
      mean_monthly = mean(flights),
      mean_daily = mean(flights_per_observed_day),
      daily_sd = sd(flights_per_observed_day),
      .groups = "drop"
    )

  result |>
    left_join(averages, by = "origin") |>
    group_by(origin) |>
    mutate(
      peak_threshold = mean_daily + daily_sd,
      peak = complete_month & flights_per_observed_day >= peak_threshold,
      previous_date = lag(month_date),
      previous_rate = lag(flights_per_observed_day),
      previous_complete = lag(complete_month, default = FALSE),
      consecutive = month_date == (previous_date %m+% months(1)),
      growth_pct = if_else(
        complete_month & previous_complete & coalesce(consecutive, FALSE),
        100 * (flights_per_observed_day / previous_rate - 1),
        NA_real_
      )
    ) |>
    ungroup()
}

metric_card <- function(title, value, detail = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(value, inline = TRUE)),
    if (!is.null(detail)) div(class = "metric-detail", textOutput(detail, inline = TRUE))
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      body { background: #f8fafc; color: #0f172a; }
      .container-fluid { max-width: 1500px; padding: 24px 30px; }
      .app-title { font-weight: 750; letter-spacing: -0.03em; margin-bottom: 4px; }
      .app-subtitle { color: #64748b; margin-bottom: 22px; }
      .well { background: white; border: 1px solid #e2e8f0; border-radius: 14px; box-shadow: none; }
      .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0,1fr)); gap: 14px; margin-bottom: 18px; }
      .metric-card { background: white; border: 1px solid #e2e8f0; border-radius: 14px; padding: 16px 18px; }
      .metric-title { color: #64748b; font-size: 13px; }
      .metric-value { font-size: 27px; font-weight: 750; margin-top: 5px; }
      .metric-detail { color: #475569; font-size: 12px; margin-top: 3px; }
      .analysis-card { background: white; border: 1px solid #e2e8f0; border-radius: 14px; padding: 16px 18px; margin-bottom: 18px; }
      .analysis-card h4 { margin-top: 0; }
      .analysis-image { display: block; width: 100%; height: auto; margin: 10px auto 14px; }
      .analysis-interpretation { color: #475569; margin-bottom: 0; line-height: 1.55; }
      .nav-tabs { border-bottom: 1px solid #cbd5e1; }
      .tab-content { background: white; border: 1px solid #e2e8f0; border-top: 0; padding: 18px; border-radius: 0 0 14px 14px; }
      @media(max-width: 900px) { .metric-grid { grid-template-columns: repeat(2,1fr); } }
    '))
  ),
  h1("Trafic aérien au départ de NYC", class = "app-title"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtres"),
      checkboxGroupInput(
        "airports", "Aéroports",
        choices = c("EWR", "JFK", "LGA"),
        selected = c("EWR", "JFK", "LGA"),
        inline = TRUE
      ),
      sliderInput(
        "coverage", "Couverture minimale d’un mois",
        min = 50, max = 100, value = 90, step = 5, post = " %"
      ),
      actionButton("refresh", "Actualiser MongoDB", icon = icon("rotate"), class = "btn-primary"),
      hr(),
      p("Les mois incomplets restent visibles dans l’onglet qualité, mais sont exclus des moyennes, croissances et pics.", class = "text-muted")
    ),
    mainPanel(
      width = 9,
      div(
        class = "metric-grid",
        metric_card("Vols — mois complets", "kpi_flights"),
        metric_card("Moyenne quotidienne", "kpi_daily"),
        metric_card("Nombre de pics", "kpi_peaks"),
        metric_card("Plus forte activité", "kpi_best", "kpi_best_detail")
      ),
      tabsetPanel(
        tabPanel(
          "Évolution",
          radioButtons(
            "metric", "Mesure",
            choices = c(
              "Vols par jour observé" = "daily",
              "Nombre mensuel de vols" = "monthly"
            ),
            selected = "daily", inline = TRUE
          ),
          plotlyOutput("trend_plot", height = "550px"),
          h4("Périodes de pic"),
          DTOutput("peaks_table")
        ),
        tabPanel("Croissance", plotlyOutput("growth_plot", height = "650px")),
        tabPanel("Carte de chaleur", plotlyOutput("heatmap_plot", height = "420px")),
        tabPanel(
          "Qualité des données",
          uiOutput("coverage_warning"),
          DTOutput("quality_table")
        ),
        tabPanel("Jours spéciaux", jours_speciaux_ui("jours_speciaux")),
        tabPanel("Analyse des retards", analyse_retards_ui("analyse_retards")),
        tabPanel("Rattrapage des retards", rattrapage_retards_ui("rattrapage_retards")),
        tabPanel("Vitesse & distance", vitesse_distance_ui("vitesse_distance")),
        tabPanel("Distance & retard", distance_retard_ui("distance_retard")),
        tabPanel("Périodes spéciales", specialPeriodsUI("special"))
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    invalidateLater(300000, session)
    input$refresh
    withProgress(message = "Lecture de MongoDB", value = 0.5, fetch_monthly())
  })

  metrics <- reactive({
    req(length(input$airports) > 0)
    calculate_metrics(raw_data(), input$coverage) |>
      filter(origin %in% input$airports)
  })

  valid <- reactive(metrics() |> filter(complete_month))
  peaks <- reactive(metrics() |> filter(peak))

  output$kpi_flights <- renderText(format(sum(valid()$flights), big.mark = " ", scientific = FALSE))
  output$kpi_daily <- renderText(sprintf("%.1f", mean(valid()$flights_per_observed_day)))
  output$kpi_peaks <- renderText(nrow(peaks()))
  output$kpi_best <- renderText({
    x <- valid() |> slice_max(flights_per_observed_day, n = 1, with_ties = FALSE)
    if (nrow(x) == 0L) "ND" else paste(x$origin, format(x$month_date, "%b %Y"), sep = " — ")
  })
  output$kpi_best_detail <- renderText({
    x <- valid() |> slice_max(flights_per_observed_day, n = 1, with_ties = FALSE)
    if (nrow(x) == 0L) "" else sprintf("%.1f vols/jour", x$flights_per_observed_day)
  })

  output$trend_plot <- renderPlotly({
    df <- valid()
    req(nrow(df) > 0)
    y <- if (input$metric == "daily") "flights_per_observed_day" else "flights"
    mean_y <- if (input$metric == "daily") "mean_daily" else "mean_monthly"
    y_label <- if (input$metric == "daily") "Vols par jour observé" else "Nombre de vols"
    mean_df <- df |>
      transmute(origin, mean_value = .data[[mean_y]]) |>
      distinct()
    p <- ggplot(df, aes(x = month_date, y = .data[[y]], color = origin, group = origin)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.5) +
      geom_hline(data = mean_df, aes(yintercept = mean_value, color = origin), linetype = "dashed", alpha = 0.5, show.legend = FALSE) +
      geom_point(data = df |> filter(peak), shape = 21, size = 5, stroke = 1.2, fill = "#FDE047") +
      scale_color_manual(values = colors) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(x = NULL, y = y_label, color = "Aéroport") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
    ggplotly(p, tooltip = c("x", "y", "color")) |> layout(hovermode = "x unified")
  })

  output$growth_plot <- renderPlotly({
    df <- metrics() |> filter(!is.na(growth_pct))
    req(nrow(df) > 0)
    p <- ggplot(df, aes(month_date, growth_pct, fill = growth_pct >= 0)) +
      geom_col(width = 24) +
      geom_hline(yintercept = 0, color = "#475569") +
      facet_wrap(~origin, ncol = 1) +
      scale_fill_manual(values = c(`TRUE` = "#16A34A", `FALSE` = "#DC2626"), guide = "none") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      labs(x = NULL, y = "Croissance mensuelle") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$heatmap_plot <- renderPlotly({
    df <- metrics() |>
      arrange(month_date) |>
      mutate(month_label = factor(format(month_date, "%b %Y"), levels = unique(format(month_date, "%b %Y"))))
    p <- ggplot(df, aes(month_label, origin, fill = flights_per_observed_day)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = if_else(is.na(flights_per_observed_day), "ND", sprintf("%.0f", flights_per_observed_day))), size = 3.8) +
      scale_fill_gradient(low = "#DBEAFE", high = "#1D4ED8", na.value = "#E2E8F0") +
      labs(x = NULL, y = NULL, fill = "Vols/jour") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })

  output$peaks_table <- renderDT({
    peaks() |>
      transmute(
        Aéroport = origin,
        Mois = format(month_date, "%B %Y"),
        Vols = flights,
        `Vols/jour` = round(flights_per_observed_day, 1),
        `Croissance (%)` = round(growth_pct, 1)
      ) |>
      datatable(rownames = FALSE, options = list(pageLength = 8, dom = "tip"))
  })

  output$quality_table <- renderDT({
    metrics() |>
      transmute(
        Aéroport = origin,
        Mois = format(month_date, "%B %Y"),
        Vols = flights,
        `Jours observés` = observed_days,
        `Jours calendrier` = calendar_days,
        `Couverture (%)` = round(coverage_pct, 1),
        Statut = status
      ) |>
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 12))
  })

  output$coverage_warning <- renderUI({
    incomplete <- metrics() |> filter(status != "Complet")
    if (nrow(incomplete) == 0L) return(NULL)
    div(class = "alert alert-warning", "Les périodes incomplètes sont exclues des moyennes, croissances et pics.")
  })
   specialPeriodsServer(
       "special",
       mongo_uri = mongo_uri,
       database = mongo_db,
       selected_airports = reactive(input$airports),
       refresh_signal = reactive(input$refresh),
       colors = colors
     )
  jours_speciaux_server("jours_speciaux", mongo_uri, mongo_db)
  analyse_retards_server("analyse_retards", mongo_uri, mongo_db)
  rattrapage_retards_server("rattrapage_retards", mongo_uri, mongo_db)
  vitesse_distance_server("vitesse_distance", mongo_uri, mongo_db)
  distance_retard_server("distance_retard", mongo_uri, mongo_db)
}

shinyApp(ui, server)

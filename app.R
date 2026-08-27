

library(shiny)
library(mongolite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# CONNEXION A MONGODB (identifiants via variables d'environnement)

get_flights_collection <- function() {
  mongo_host <- Sys.getenv("MONGO_HOST", "127.0.0.1")
  mongo_port <- Sys.getenv("MONGO_PORT", "27018")
  mongo_user <- Sys.getenv("MONGO_APP_USER", "adp_app")
  mongo_pwd  <- Sys.getenv("MONGO_APP_PASSWORD", "Laazibi1234")
  mongo_db   <- "nyc_flights"

  mongo_url <- sprintf(
    "mongodb://%s:%s@%s:%s/%s?authSource=%s",
    mongo_user, mongo_pwd, mongo_host, mongo_port, mongo_db, mongo_db
  )

  mongo(collection = "flights", db = mongo_db, url = mongo_url)
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(

  titlePanel("✈️ Trafic aérien au départ de NYC — Dashboard ADP"),
  h5("Tableau de bord R Shiny connecté directement à MongoDB", style = "color: grey;"),

  sidebarLayout(

    sidebarPanel(
      width = 3,

      h4("Filtres"),
      checkboxGroupInput(
        "aeroports", "Aéroports d'origine :",
        choices  = c("EWR", "JFK", "LGA"),
        selected = c("EWR", "JFK", "LGA")
      ),

      sliderInput(
        "mois_range", "Mois :",
        min = 1, max = 12, value = c(1, 12), step = 1
      ),

      actionButton("actualiser", "🔄 Actualiser depuis MongoDB", class = "btn-primary"),

      hr(),
      helpText("Les filtres s'appliquent à tous les onglets. Clique sur",
               "'Actualiser' après avoir changé les filtres.")
    ),

    mainPanel(
      width = 9,

      fluidRow(
        column(3, wellPanel(h5("Vols (période)"), textOutput("kpi_nb_vols"))),
        column(3, wellPanel(h5("Retard moyen dép."), textOutput("kpi_retard_moyen"))),
        column(3, wellPanel(h5("Taux d'annulation"), textOutput("kpi_taux_annulation"))),
        column(3, wellPanel(h5("Aéroport + fiable"), textOutput("kpi_meilleur_aeroport")))
      ),

      tabsetPanel(

        tabPanel("Évolution du trafic",
          plotOutput("plot_trafic_mensuel", height = "450px")
        ),

        tabPanel("Retards",
          fluidRow(
            column(6, plotOutput("plot_retard_aeroport", height = "400px")),
            column(6, plotOutput("plot_retard_heure", height = "400px"))
          )
        ),

        tabPanel("Vols annulés",
          plotOutput("plot_annulations_carrier", height = "450px")
        ),

        tabPanel("Prévision (simple)",
          p("Extrapolation linéaire simple du nombre de vols mensuels.",
            "Modèle basique (régression linéaire), ne tient pas compte",
            "de la saisonnalité complexe - à but illustratif seulement."),
          plotOutput("plot_prevision", height = "450px")
        )
      )
    )
  )
)


# SERVER

server <- function(input, output, session) {

  # Recharge les données depuis MongoDB à chaque clic sur "Actualiser"
  donnees <- eventReactive(input$actualiser, {
    req(input$aeroports)

    col <- get_flights_collection()


    query <- sprintf('{"origin": {"$in": [%s]}}',
                      paste0('"', input$aeroports, '"', collapse = ","))

    fields <- '{"origin":1, "dest":1, "carrier":1, "dep_delay":1, "arr_delay":1,
                "cancelled":1, "time_hour":1, "sched_dep_datetime":1, "_id":0}'

    d <- col$find(query = query, fields = fields)

    d$time_hour           <- as.POSIXct(d$time_hour, tz = "UTC")
    d$sched_dep_datetime  <- as.POSIXct(d$sched_dep_datetime, tz = "UTC")
    d$mois      <- month(d$time_hour)
    d$heure_dep <- hour(d$sched_dep_datetime)

    d %>% filter(mois >= input$mois_range[1], mois <= input$mois_range[2])
  }, ignoreNULL = FALSE)


  # --- KPI ---

  output$kpi_nb_vols <- renderText({
    format(nrow(donnees()), big.mark = " ")
  })

  output$kpi_retard_moyen <- renderText({
    d <- donnees()
    paste0(round(mean(d$dep_delay, na.rm = TRUE), 1), " min")
  })

  output$kpi_taux_annulation <- renderText({
    d <- donnees()
    paste0(round(mean(d$cancelled) * 100, 2), " %")
  })

  output$kpi_meilleur_aeroport <- renderText({
    d <- donnees()
    if (nrow(d) == 0) return("N/A")
    d %>%
      group_by(origin) %>%
      summarise(retard = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
      arrange(retard) %>%
      slice(1) %>%
      pull(origin)
  })


  # --- Onglet Evolution du trafic ---

  output$plot_trafic_mensuel <- renderPlot({
    d <- donnees()
    req(nrow(d) > 0)

    trafic <- d %>% count(origin, mois, name = "nb_vols")

    ggplot(trafic, aes(x = factor(mois), y = nb_vols, color = origin, group = origin)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = "Nombre de vols par mois, par aéroport d'origine",
        x = "Mois", y = "Nombre de vols", color = "Aéroport"
      ) +
      theme_minimal(base_size = 13)
  })


  # --- Onglet Retards ---

  output$plot_retard_aeroport <- renderPlot({
    d <- donnees()
    req(nrow(d) > 0)

    d %>%
      filter(!is.na(dep_delay)) %>%
      group_by(origin) %>%
      summarise(retard_moyen = mean(dep_delay), .groups = "drop") %>%
      ggplot(aes(x = reorder(origin, retard_moyen), y = retard_moyen, fill = origin)) +
      geom_col() +
      labs(title = "Retard moyen au départ, par aéroport", x = NULL, y = "Retard moyen (min)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$plot_retard_heure <- renderPlot({
    d <- donnees()
    req(nrow(d) > 0)

    d %>%
      filter(!is.na(dep_delay)) %>%
      group_by(heure_dep) %>%
      summarise(retard_moyen = mean(dep_delay), nb_vols = n(), .groups = "drop") %>%
      filter(nb_vols >= 50) %>%
      ggplot(aes(x = heure_dep, y = retard_moyen)) +
      geom_col(fill = "steelblue") +
      labs(title = "Retard moyen selon l'heure de décollage",
           subtitle = "Heures avec < 50 vols exclues",
           x = "Heure", y = "Retard moyen (min)") +
      theme_minimal(base_size = 13)
  })


  # --- Onglet Vols annulés ---

  output$plot_annulations_carrier <- renderPlot({
    d <- donnees()
    req(nrow(d) > 0)

    d %>%
      filter(cancelled == TRUE) %>%
      count(carrier, name = "nb_annulations") %>%
      arrange(desc(nb_annulations)) %>%
      ggplot(aes(x = reorder(carrier, nb_annulations), y = nb_annulations)) +
      geom_col(fill = "firebrick") +
      coord_flip() +
      labs(title = "Nombre de vols annulés par compagnie", x = NULL, y = "Annulations") +
      theme_minimal(base_size = 13)
  })


  # --- Onglet Prévision ---

  output$plot_prevision <- renderPlot({
    d <- donnees()
    req(nrow(d) > 0)

    trafic_mensuel <- d %>%
      count(mois, name = "nb_vols") %>%
      arrange(mois)

    req(nrow(trafic_mensuel) >= 3)

    modele <- lm(nb_vols ~ mois, data = trafic_mensuel)

    mois_futurs <- data.frame(mois = (max(trafic_mensuel$mois) + 1):(max(trafic_mensuel$mois) + 3))
    mois_futurs <- mois_futurs %>% filter(mois <= 12)

    if (nrow(mois_futurs) > 0) {
      mois_futurs$nb_vols <- predict(modele, newdata = mois_futurs)
      mois_futurs$type <- "Prévision"
    }
    trafic_mensuel$type <- "Observé"

    combine <- bind_rows(trafic_mensuel, mois_futurs)

    ggplot(combine, aes(x = mois, y = nb_vols, color = type)) +
      geom_line(data = combine %>% filter(type == "Observé"), linewidth = 1) +
      geom_point(size = 2) +
      geom_line(data = combine %>% filter(mois >= max(trafic_mensuel$mois)),
                linetype = "dashed") +
      scale_color_manual(values = c("Observé" = "steelblue", "Prévision" = "firebrick")) +
      labs(
        title = "Trafic mensuel observé + prévision (régression linéaire simple)",
        x = "Mois", y = "Nombre de vols", color = NULL
      ) +
      theme_minimal(base_size = 13)
  })

}


# LANCEMENT


shinyApp(ui = ui, server = server)
# =============================================================================
# Module Shiny : "Analyse des retards au départ et à l'arrivée"
#
# Les indicateurs sont recalculés depuis la collection flights nettoyée.
# Les graphiques PNG proviennent de script_r/analyse_retards.R et sont servis
# depuis le dossier www/output de l'application Shiny.
# =============================================================================

analyse_retards_ui <- function(id) {
  ns <- NS(id)

  figure_retards <- function(titre, fichier, texte_alternatif, interpretation) {
    div(
      class = "analysis-card",
      h4(titre),
      tags$img(
        src = file.path("output", fichier),
        alt = texte_alternatif,
        class = "analysis-image"
      ),
      p(
        strong("Interprétation : "),
        interpretation,
        class = "analysis-interpretation"
      )
    )
  }

  tagList(
    p(
      paste(
        "Les indicateurs sont calculés à partir des retards renseignés dans",
        "la collection MongoDB nettoyée. Les valeurs manquantes restent",
        "conservées dans les données et sont ignorées uniquement lorsque le",
        "calcul ne peut pas être effectué."
      ),
      class = "text-muted"
    ),
    div(
      class = "metric-grid",
      div(
        class = "metric-card",
        div(class = "metric-title", "Retard moyen au départ"),
        div(class = "metric-value", textOutput(ns("kpi_retard_moyen"), inline = TRUE))
      ),
      div(
        class = "metric-card",
        div(class = "metric-title", "Arrivée > 2 h après un départ à l'heure"),
        div(class = "metric-value", textOutput(ns("kpi_arrivee_2h"), inline = TRUE))
      ),
      div(
        class = "metric-card",
        div(class = "metric-title", "Vols partis en avance"),
        div(class = "metric-value", textOutput(ns("kpi_depart_avance"), inline = TRUE))
      ),
      div(
        class = "metric-card",
        div(class = "metric-title", "Vols arrivés en avance"),
        div(class = "metric-value", textOutput(ns("kpi_arrivee_avance"), inline = TRUE))
      )
    ),
    fluidRow(
      column(
        6,
        figure_retards(
          "Distribution des retards au départ",
          "distribution_retards_depart.png",
          "Histogramme de la distribution des retards au départ",
          paste(
            "La majorité des départs se situe près de l'horaire prévu.",
            "La longue traîne vers la droite correspond à un nombre limité",
            "de vols ayant subi des retards importants."
          )
        )
      ),
      column(
        6,
        figure_retards(
          "Distribution des retards à l'arrivée",
          "distribution_retards_arrivee.png",
          "Histogramme de la distribution des retards à l'arrivée",
          paste(
            "De nombreux vols arrivent à l'heure ou en avance, ce qui se",
            "traduit par des valeurs proches de zéro ou négatives. Quelques",
            "retards élevés expliquent l'asymétrie de la distribution."
          )
        )
      )
    ),
    figure_retards(
      "Évolution du retard moyen journalier au départ",
      "retard_moyen_journalier_depart.png",
      "Courbe du retard moyen journalier au départ",
      paste(
        "Le retard moyen varie fortement selon les jours et présente plusieurs",
        "pics ponctuels. Les interruptions de la courbe correspondent à des",
        "dates absentes du jeu de données et non à un retard nul."
      )
    )
  )
}

analyse_retards_server <- function(id, mongo_uri, mongo_db) {
  moduleServer(id, function(input, output, session) {

    indicateurs_retards <- reactive({
      con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)

      pipeline <- '[
        {"$group":{
          "_id":null,
          "retard_moyen_depart":{"$avg":"$dep_delay_min"},
          "arrivee_2h_apres_depart_heure":{"$sum":{"$cond":[
            {"$and":[
              {"$ne":["$dep_delay_min",null]},
              {"$ne":["$arr_delay_min",null]},
              {"$lte":["$dep_delay_min",0]},
              {"$gt":["$arr_delay_min",120]}
            ]},1,0
          ]}},
          "depart_en_avance":{"$sum":{"$cond":[
            {"$and":[
              {"$ne":["$dep_delay_min",null]},
              {"$lt":["$dep_delay_min",0]}
            ]},1,0
          ]}},
          "arrivee_en_avance":{"$sum":{"$cond":[
            {"$and":[
              {"$ne":["$arr_delay_min",null]},
              {"$lt":["$arr_delay_min",0]}
            ]},1,0
          ]}}
        }},
        {"$project":{"_id":0}}
      ]'

      donnees <- con$aggregate(pipeline) |> as_tibble()
      validate(need(nrow(donnees) == 1L, "Aucun indicateur de retard disponible."))
      donnees
    })

    format_entier <- function(valeur) {
      format(as.integer(valeur), big.mark = " ", scientific = FALSE)
    }

    output$kpi_retard_moyen <- renderText({
      valeur <- indicateurs_retards()$retard_moyen_depart[[1]]
      paste0(formatC(valeur, format = "f", digits = 2, decimal.mark = ","), " min")
    })

    output$kpi_arrivee_2h <- renderText({
      format_entier(indicateurs_retards()$arrivee_2h_apres_depart_heure[[1]])
    })

    output$kpi_depart_avance <- renderText({
      format_entier(indicateurs_retards()$depart_en_avance[[1]])
    })

    output$kpi_arrivee_avance <- renderText({
      format_entier(indicateurs_retards()$arrivee_en_avance[[1]])
    })
  })
}

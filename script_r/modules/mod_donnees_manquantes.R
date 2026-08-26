# =============================================================================
# Module Shiny : "Analyser les données manquantes"
# Version interactive de script_r/analyse_donnees_manquantes.R.
#
# Les indicateurs sont calculés directement dans MongoDB. Les champs absents et
# les valeurs BSON null sont tous deux comptés comme des données manquantes.
# =============================================================================

donnees_manquantes_card <- function(ns, title, value_id, detail_id = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(ns(value_id), inline = TRUE)),
    if (!is.null(detail_id)) {
      div(class = "metric-detail", textOutput(ns(detail_id), inline = TRUE))
    }
  )
}

donnees_manquantes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      paste0(
        "Bilan des valeurs manquantes dans la collection nettoyée. ",
        "Les indicateurs sont recalculés depuis MongoDB."
      ),
      class = "text-muted"
    ),
    div(
      class = "metric-grid",
      donnees_manquantes_card(ns, "Nombre total de vols", "kpi_total"),
      donnees_manquantes_card(ns, "dep_time manquants", "kpi_dep_time", "pct_dep_time"),
      donnees_manquantes_card(ns, "arr_time manquants", "kpi_arr_time", "pct_arr_time"),
      donnees_manquantes_card(ns, "dep_delay manquants", "kpi_dep_delay", "pct_dep_delay"),
      donnees_manquantes_card(ns, "arr_delay manquants", "kpi_arr_delay", "pct_arr_delay"),
      donnees_manquantes_card(ns, "air_time manquants", "kpi_air_time", "pct_air_time")
    ),
    h4("Pourcentage de valeurs manquantes par variable"),
    div(
      style = "text-align: center;",
      tags$img(
        src = "output/pourcentage_valeurs_manquantes.png",
        alt = "Pourcentage de valeurs manquantes par variable",
        style = "width: 100%; max-width: 900px; height: auto;"
      )
    ),
    div(
      class = "alert alert-info",
      h4("Interprétation"),
      p(
        paste0(
          "Les taux de valeurs manquantes restent relativement faibles. ",
          "Les variables arr_delay et air_time sont les plus touchées."
        )
      ),
      p(
        paste0(
          "Ces absences peuvent avoir une signification métier. En particulier, ",
          "les cas où dep_time et arr_time sont simultanément manquants peuvent ",
          "correspondre à des vols potentiellement annulés. Cette hypothèse n'est ",
          "pas considérée ici comme une classification définitive : elle devra être ",
          "confirmée dans l'analyse dédiée aux annulations."
        )
      )
    )
  )
}

donnees_manquantes_server <- function(id, mongo_uri, mongo_db) {
  moduleServer(id, function(input, output, session) {

    statistiques_na <- reactive({
      invalidateLater(300000, session)

      con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
      pipeline <- '[
        {"$group":{
          "_id":null,
          "total":{"$sum":1},
          "dep_time_na":{"$sum":{"$cond":[{"$in":[{"$type":"$dep_time"},["missing","null"]]},1,0]}},
          "arr_time_na":{"$sum":{"$cond":[{"$in":[{"$type":"$arr_time"},["missing","null"]]},1,0]}},
          "dep_delay_na":{"$sum":{"$cond":[{"$in":[{"$type":"$dep_delay_min"},["missing","null"]]},1,0]}},
          "arr_delay_na":{"$sum":{"$cond":[{"$in":[{"$type":"$arr_delay_min"},["missing","null"]]},1,0]}},
          "air_time_na":{"$sum":{"$cond":[{"$in":[{"$type":"$air_time_min"},["missing","null"]]},1,0]}}
        }},
        {"$project":{"_id":0}}
      ]'

      statistiques <- con$aggregate(pipeline) |> as_tibble()
      validate(need(nrow(statistiques) == 1L, "Collection flights vide."))
      statistiques
    })

    afficher_nombre <- function(colonne) {
      renderText({
        valeur <- statistiques_na()[[colonne]][[1]]
        format(valeur, big.mark = " ", scientific = FALSE)
      })
    }

    afficher_pourcentage <- function(colonne) {
      renderText({
        statistiques <- statistiques_na()
        pourcentage <- 100 * statistiques[[colonne]][[1]] / statistiques$total[[1]]
        sprintf("%.3f %% des vols", pourcentage)
      })
    }

    output$kpi_total <- afficher_nombre("total")
    output$kpi_dep_time <- afficher_nombre("dep_time_na")
    output$kpi_arr_time <- afficher_nombre("arr_time_na")
    output$kpi_dep_delay <- afficher_nombre("dep_delay_na")
    output$kpi_arr_delay <- afficher_nombre("arr_delay_na")
    output$kpi_air_time <- afficher_nombre("air_time_na")

    output$pct_dep_time <- afficher_pourcentage("dep_time_na")
    output$pct_arr_time <- afficher_pourcentage("arr_time_na")
    output$pct_dep_delay <- afficher_pourcentage("dep_delay_na")
    output$pct_arr_delay <- afficher_pourcentage("arr_delay_na")
    output$pct_air_time <- afficher_pourcentage("air_time_na")
  })
}

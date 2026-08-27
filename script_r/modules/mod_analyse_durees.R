# =============================================================================
# Module Shiny : "Analyser et convertir les données de durée"
# Version interactive de script_r/analyse_durees.R.
#
# Les indicateurs sont recalculés à partir de la collection MongoDB nettoyée.
# Les champs de durée utilisent les suffixes métier _min de cette collection.
# =============================================================================

analyse_durees_card <- function(ns, title, value_id, detail_id = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(ns(value_id), inline = TRUE)),
    if (!is.null(detail_id)) {
      div(class = "metric-detail", textOutput(ns(detail_id), inline = TRUE))
    }
  )
}

analyse_durees_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      paste0(
        "Conversion des horaires HHMM et comparaison du retard au départ ",
        "observé avec le retard recalculé."
      ),
      class = "text-muted"
    ),
    div(
      class = "metric-grid",
      analyse_durees_card(ns, "Nombre total de vols", "kpi_total"),
      analyse_durees_card(
        ns,
        "Retards comparables",
        "kpi_comparables_depart"
      ),
      analyse_durees_card(
        ns,
        "Taux de correspondance",
        "kpi_taux_correspondance",
        "detail_correspondances"
      ),
      analyse_durees_card(
        ns,
        "Incohérences significatives",
        "kpi_incoherences"
      ),
      analyse_durees_card(
        ns,
        "Retard maximal observé",
        "kpi_retard_max",
        "detail_retard_max"
      ),
      analyse_durees_card(
        ns,
        "Lignes comparables pour air_time",
        "kpi_comparables_air"
      )
    ),
    h4("Retard observé et retard recalculé"),
    div(
      style = "text-align: center;",
      tags$img(
        src = "output/comparaison_retard_depart_calcule.png",
        alt = "Comparaison du retard observé et du retard recalculé",
        style = "width: 100%; max-width: 900px; height: auto;"
      )
    ),
    div(
      class = "alert alert-info",
      h4("Interprétation"),
      p(
        paste0(
          "Les horaires dep_time et sched_dep_time sont convertis du format ",
          "HHMM en nombre de minutes depuis minuit. Après correction simple ",
          "du passage à minuit, le retard recalculé correspond presque ",
          "toujours au dep_delay observé."
        )
      ),
      p(
        paste0(
          "Quelques écarts de 1 440 minutes peuvent apparaître pour des ",
          "retards très longs : les seules heures HHMM ne permettent pas ",
          "toujours de déterminer avec certitude le jour du départ réel."
        )
      ),
      p(
        paste0(
          "air_time ne doit pas être remplacé naïvement par arr_time - ",
          "dep_time. Le passage à minuit, les fuseaux horaires et les temps ",
          "au sol influencent cette durée apparente. air_time reste la mesure ",
          "la plus adaptée du temps réellement passé en vol."
        )
      )
    )
  )
}

analyse_durees_server <- function(id, mongo_uri, mongo_db) {
  moduleServer(id, function(input, output, session) {

    convertir_hhmm_minutes <- function(horaire) {
      valeur <- suppressWarnings(as.integer(horaire))
      heures <- valeur %/% 100L
      minutes <- valeur %% 100L

      horaire_valide <- !is.na(valeur) & (
        (heures >= 0L & heures <= 23L & minutes >= 0L & minutes <= 59L) |
          valeur == 2400L
      )

      resultat <- rep(NA_integer_, length(valeur))
      resultat[horaire_valide] <-
        heures[horaire_valide] * 60L + minutes[horaire_valide]
      resultat
    }

    corriger_passage_minuit <- function(ecart_minutes) {
      case_when(
        is.na(ecart_minutes) ~ NA_real_,
        ecart_minutes < -720 ~ ecart_minutes + 1440,
        ecart_minutes > 720 ~ ecart_minutes - 1440,
        TRUE ~ as.numeric(ecart_minutes)
      )
    }

    statistiques_durees <- reactive({
      invalidateLater(300000, session)

      con <- mongo(collection = "flights", db = mongo_db, url = mongo_uri)
      vols <- con$find(
        query = "{}",
        fields = paste0(
          '{"_id":0,"dep_time":1,"sched_dep_time":1,',
          '"dep_delay_min":1,"arr_time":1,"air_time_min":1}'
        )
      ) |>
        as_tibble()

      validate(need(nrow(vols) > 0L, "Collection flights vide."))

      # Si un champ est absent de tous les documents MongoDB, mongolite peut
      # ne pas créer la colonne. On l'ajoute alors comme une colonne de NA.
      colonnes_requises <- c(
        "dep_time", "sched_dep_time", "dep_delay_min",
        "arr_time", "air_time_min"
      )
      for (nom in setdiff(colonnes_requises, names(vols))) {
        vols[[nom]] <- NA_real_
      }

      vols <- vols |>
        mutate(
          dep_minutes = convertir_hhmm_minutes(dep_time),
          sched_dep_minutes = convertir_hhmm_minutes(sched_dep_time),
          arr_minutes = convertir_hhmm_minutes(arr_time),
          ecart_depart_brut = dep_minutes - sched_dep_minutes,
          retard_depart_calcule = corriger_passage_minuit(ecart_depart_brut),
          ecart_retard = retard_depart_calcule - dep_delay_min,
          depart_comparable = !is.na(dep_delay_min) &
            !is.na(retard_depart_calcule),
          air_time_comparable = !is.na(air_time_min) &
            !is.na(dep_minutes) & !is.na(arr_minutes)
        )

      retards_connus <- vols$dep_delay_min[!is.na(vols$dep_delay_min)]
      retard_maximal <- if (length(retards_connus) == 0L) {
        NA_real_
      } else {
        max(retards_connus)
      }

      vols |>
        summarise(
          total = n(),
          comparables_depart = sum(depart_comparable),
          correspondances = sum(
            depart_comparable & abs(ecart_retard) <= 1,
            na.rm = TRUE
          ),
          incoherences = sum(
            depart_comparable & abs(ecart_retard) > 1,
            na.rm = TRUE
          ),
          retard_max = retard_maximal,
          comparables_air = sum(air_time_comparable)
        ) |>
        mutate(
          taux_correspondance = if_else(
            comparables_depart > 0,
            100 * correspondances / comparables_depart,
            NA_real_
          )
        )
    })

    afficher_nombre <- function(colonne) {
      renderText({
        valeur <- statistiques_durees()[[colonne]][[1]]
        if (is.na(valeur)) return("ND")
        format(valeur, big.mark = " ", scientific = FALSE)
      })
    }

    output$kpi_total <- afficher_nombre("total")
    output$kpi_comparables_depart <- afficher_nombre("comparables_depart")
    output$kpi_incoherences <- afficher_nombre("incoherences")
    output$kpi_retard_max <- afficher_nombre("retard_max")
    output$kpi_comparables_air <- afficher_nombre("comparables_air")

    output$kpi_taux_correspondance <- renderText({
      taux <- statistiques_durees()$taux_correspondance[[1]]
      if (is.na(taux)) "ND" else sprintf("%.3f %%", taux)
    })

    output$detail_correspondances <- renderText({
      nombre <- statistiques_durees()$correspondances[[1]]
      paste(format(nombre, big.mark = " "), "correspondances")
    })

    output$detail_retard_max <- renderText("minutes")
  })
}

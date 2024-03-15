#' synthese_toutes_stations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_synthese_toutes_stations_ui <- function(id){
  ns <- NS(id)

  # css <- HTML(
  #   paste0(".sub-tabpanel {
  #              height-max: 350px;
  #          }")
  # )

  tagList(
    # tags$head(
    #   tags$style(css)
    # ),
    uiOutput(ns("synthese_stations"))
  )
}

#' synthese_toutes_stations Server Functions
#'
#' @noRd
mod_synthese_toutes_stations_server <- function(id, stations, indices, choix_departement, choix_eqb, suivi_regie){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    indices_dep <- indices %>%
      dplyr::left_join(
        stations %>%
          dplyr::select(code_station_hydrobio, code_departement, regie),
        by = "code_station_hydrobio"
      )

    observe({
      req(choix_departement, choix_eqb, suivi_regie)

      if (suivi_regie()) {
        indices_dep <- indices_dep %>%
          dplyr::filter(regie)
      }

      deps <- choix_departement()
      if (is.null(deps))
        deps <- unique(indices_dep$code_departement)
      if ("PPC" %in% deps)
        deps <- c(deps[deps != "PPC"], 75, 92, 93, 94)

      eqb <- choix_eqb()
      if (is.null(eqb))
        eqb <- unique(indices_dep$code_support)

      resume_indices <- indices_dep %>%
        dplyr::filter(
          code_departement %in% deps,
          code_support %in% eqb
          )

      mod_hist_chroniques_server(
        id = "chroniques",
        donnees_graphique = resume_indices
        )

      output$synthese_stations <- renderUI({

        tags$div(
          class = "sub-tabpanel",
          tabsetPanel(
            tabPanel(
              title = "Chroniques",
              mod_hist_chroniques_ui(ns("chroniques"))
            ),
            tabPanel(
              title = "Qualité"
            )
          )
        )

      })
    })
  })
}

## To be copied in the UI
# mod_synthese_toutes_stations_ui("synthese_toutes_stations_1")

## To be copied in the server
# mod_synthese_toutes_stations_server("synthese_toutes_stations_1")

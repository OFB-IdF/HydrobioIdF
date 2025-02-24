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
mod_synthese_toutes_stations_server <- function(id, stations, indices, choix_stations, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    indices_dep <- indices %>%
      dplyr::left_join(
        stations %>%
          dplyr::select(code_station_hydrobio, code_departement, regie),
        by = "code_station_hydrobio"
      )

    observe({
      req(choix_stations, choix_eqb)


      eqb <- choix_eqb()
      if (is.null(eqb))
        eqb <- unique(indices_dep$code_support)

      resume_indices <- indices_dep %>%
        dplyr::filter(
          code_station_hydrobio %in% choix_stations(),
          code_support %in% eqb
          )

      mod_hist_chroniques_server(
        id = "chroniques",
        donnees_graphique = resume_indices
        )

      mod_hist_qualite_server(
        id = "qualite",
        donnees_graphique = resume_indices
      )

      if (nrow(resume_indices) == 0) {
        output$synthese_stations <- renderUI(NULL)
      } else {
        output$synthese_stations <- renderUI({

          tags$div(
            class = "sub-tabpanel",
            tabsetPanel(
              tabPanel(
                title = "Qualité",
                mod_hist_qualite_ui(ns("qualite"))
              ),
              tabPanel(
                title = "Chroniques",
                mod_hist_chroniques_ui(ns("chroniques"))
              )
            )
          )

        })
      }
    })
  })
}

## To be copied in the UI
# mod_synthese_toutes_stations_ui("synthese_toutes_stations_1")

## To be copied in the server
# mod_synthese_toutes_stations_server("synthese_toutes_stations_1")

#' synthese_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_synthese_station_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("synthese_station"))
  )
}

#' synthese_station Server Functions
#'
#' @noRd
mod_synthese_station_server <- function(id, resumes_listes, stations, indices, acronymes_indices, listes_taxo, choix_station, choix_eqb, ordre_taxon, choix_stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(choix_station(), choix_stations)

      station_regie <- stations %>%
        dplyr::filter(code_station_hydrobio == choix_station()) %>%
        dplyr::pull(regie)

      if (!choix_station() %in% choix_stations()) {

        output$synthese_station <- renderUI(NULL)

      } else {
        output$synthese_station <- renderUI({
          req(choix_station, choix_eqb)

          mod_chiffres_cles_station_server(
            id = "chiffres_cles",
            resumes_listes, stations,
            choix_station, choix_eqb
          )

          mod_chronique_indices_server(
            id = "chronique_indices",
            stations, indices, acronymes_indices,
            choix_station, choix_eqb
          )

          mod_chronique_taxons_server(
            id = "chronique_taxons",
            stations, listes_taxo,
            choix_station, choix_eqb, ordre_taxon
          )

          if (is.null(choix_station())) {
            div()
          } else {

            div(
              mod_chiffres_cles_station_ui(id = ns("chiffres_cles")),
              tags$div(
                class = "sub-tabpanel",
                tabsetPanel(
                  tabPanel(
                    title = "Indices",
                    mod_chronique_indices_ui(id = ns("chronique_indices"))
                  ),
                  tabPanel(
                    title = "Taxons",
                    mod_chronique_taxons_ui(id = ns("chronique_taxons"))
                  )
                )
              )
            )
          }

        })
      }
    })
  })
}

## To be copied in the UI
# mod_synthese_station_ui("synthese_station_1")

## To be copied in the server
# mod_synthese_station_server("synthese_station_1")

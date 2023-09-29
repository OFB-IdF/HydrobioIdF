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
mod_synthese_station_server <- function(id, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(choix_station, choix_eqb)

      output$synthese_station <- renderUI({

        mod_chiffres_cles_station_server(
          id = "chiffres_cles",
          choix_station, choix_eqb
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
                  title = "Indices"
                ),
                tabPanel(
                  title = "Taxons"
                )
              )
            )
          )
        }

      })
    })
  })
}

## To be copied in the UI
# mod_synthese_station_ui("synthese_station_1")

## To be copied in the server
# mod_synthese_station_server("synthese_station_1")

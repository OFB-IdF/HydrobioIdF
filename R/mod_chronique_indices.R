#' chronique_indices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
mod_chronique_indices_ui <- function(id){
  ns <- NS(id)

  css <- HTML(
    paste0(".box-body {
               max-height: 500px !important;
               min-width: 350px !important;
               overflow-y: scroll;
           }")
  )

  tagList(
    tags$head(
      tags$style(css)
    ),
    shinydashboard::box(
      plotOutput(ns("indices"), height = 500)
    )
  )
}

#' chronique_indices Server Functions
#'
#' @noRd
mod_chronique_indices_server <- function(id, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(choix_station, choix_eqb)

      DonneesGraphique <- filtrer_indices(choix_station(), choix_eqb())

      output$indices <- renderPlot({
        tracer_chroniques_indices(DonneesGraphique)
      },
      height = dplyr::n_distinct(DonneesGraphique$code_indice) * 250
      )
    })
  })
}

## To be copied in the UI
# mod_chronique_indices_ui("chronique_indices_1")

## To be copied in the server
# mod_chronique_indices_server("chronique_indices_1")

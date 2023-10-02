#' hist_chroniques UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_hist_chroniques_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("chroniques"))
  )
}

#' hist_chroniques Server Functions
#'
#' @noRd
#' @importFrom plotly renderPlotly
mod_hist_chroniques_server <- function(id, donnees_graphique){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$chroniques <- plotly::renderPlotly({
      plot_chroniques(
        donnees_graphique,
        interactive = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_hist_chroniques_ui("hist_chroniques_1")

## To be copied in the server
# mod_hist_chroniques_server("hist_chroniques_1")

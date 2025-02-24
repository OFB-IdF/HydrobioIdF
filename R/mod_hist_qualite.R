#' hist_qualite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_hist_qualite_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("qualite"), height = "350px")
  )
}

#' hist_qualite Server Functions
#'
#' @noRd
#' @importFrom plotly renderPlotly
mod_hist_qualite_server <- function(id, donnees_graphique){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$qualite <- plotly::renderPlotly({
      plot_qualite(
        donnees_graphique,
        interactive = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_hist_qualite_ui("hist_qualite_1")

## To be copied in the server
# mod_hist_qualite_server("hist_qualite_1")

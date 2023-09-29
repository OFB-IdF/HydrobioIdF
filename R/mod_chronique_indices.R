#' chronique_indices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chronique_indices_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("indices"), height = 500)
  )
}

#' chronique_indices Server Functions
#'
#' @noRd
mod_chronique_indices_server <- function(id, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        if (length(unique(na.omit(x))) == 1) {
          breaks <- unique(na.omit(x))
        } else {
          breaks <- floor(pretty(x, n, ...))
        }

        names(breaks) <- attr(breaks, "labels")
        unique(breaks)
      }
      return(fxn)
    }

    observe({
      req(choix_station, choix_eqb)

      DonneesGraphique <- filtrer_indices(choix_station(), choix_eqb())

      output$indices <- renderPlot({
        tracer_chroniques_indices(DonneesGraphique)
      })
    })
  })
}

## To be copied in the UI
# mod_chronique_indices_ui("chronique_indices_1")

## To be copied in the server
# mod_chronique_indices_server("chronique_indices_1")

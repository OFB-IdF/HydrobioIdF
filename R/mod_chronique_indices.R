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
  h <- 525
  w <- 550

  css <- HTML(
    paste0(".box-body {
               max-height: ", h, "px !important;
               min-width: ", w, "px !important;
               overflow-y: scroll;
           }")
  )

  tagList(
    tags$head(
      tags$style(css)
    ),
    shinydashboard::box(
      plotOutput(ns("indices"), height = h)
    )
  )
}

#' chronique_indices Server Functions
#'
#' @noRd
mod_chronique_indices_server <- function(id, stations, regie, indices, acronymes_indices, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    w <- 525

    observe({
      req(choix_station, choix_eqb)

      DonneesGraphique <- filtrer_indices(stations, indices, choix_station(), choix_eqb())

      output$indices <- renderPlot({
        tracer_chroniques_indices(DonneesGraphique, acronymes_indices, regie = regie, interactive = FALSE)
      },
      height = dplyr::n_distinct(DonneesGraphique$code_indice) * 250,
      width = w
      )
    })
  })
}

## To be copied in the UI
# mod_chronique_indices_ui("chronique_indices_1")

## To be copied in the server
# mod_chronique_indices_server("chronique_indices_1")

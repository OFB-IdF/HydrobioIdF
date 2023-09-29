#' chiffres_cles_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chiffres_cles_station_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns("titre")),
    tableOutput(ns("tableau"))
  )
}

#' chiffres_cles_station Server Functions
#'
#' @noRd
mod_chiffres_cles_station_server <- function(id, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      station <- filtrer_station(choix_station())
      indices_station <- filtrer_indices(choix_station(), choix_eqb())
      listes_station <- filtrer_listes(choix_station(), choix_eqb())

      resume_listes <- resumer_listes(listes_station)

      output$tableau <- renderTable(resume_listes)
      output$titre <- renderText(with(
        station,
        paste0('<a href="', uri_station_hydrobio, '" target=”_blank”><b>',
               libelle_station_hydrobio, '</b></a>')
        ))
    })


  })
}

## To be copied in the UI
# mod_chiffres_cles_station_ui("chiffres_cles_station_1")

## To be copied in the server
# mod_chiffres_cles_station_server("chiffres_cles_station_1")

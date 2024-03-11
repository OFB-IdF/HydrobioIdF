#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("date"))
  )
}

#' load_data Server Functions
#'
#' @noRd
mod_load_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    download.file(url = "https://github.com/OFB-IdF/HydrobioIdF/raw/main/dev/data_hydrobio.rda", destfile = "data_hydrobio.rda")
    load("data_hydrobio.rda", envir = .GlobalEnv)
    unlink("data_hydrobio.rda")

    output$date <- renderText(as.character(get("date_donnees")))

  })
}

## To be copied in the UI
# mod_load_data_ui("load_data_1")

## To be copied in the server
# mod_load_data_server("load_data_1")

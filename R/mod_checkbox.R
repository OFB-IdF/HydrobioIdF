#' checkbox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_checkbox_ui <- function(id, titre = "", defaut = FALSE){
  ns <- NS(id)
  tagList(
    checkboxInput(
      inputId = ns("checkbox"),
      label = titre,
      value = defaut
    )
  )
}

#' checkbox Server Functions
#'
#' @noRd
mod_checkbox_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive(input$checkbox)
  })
}

## To be copied in the UI
# mod_checkbox_ui("checkbox_1")

## To be copied in the server
# mod_checkbox_server("checkbox_1")

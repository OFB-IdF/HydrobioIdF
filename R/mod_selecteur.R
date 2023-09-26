#' selecteur UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selecteur_ui <- function(id, texte, choix, choix_multiple = FALSE){
  select_choices <- c("", choix)

  if (length(names(choix)) == 0) {
    names(select_choices) <- c(texte, rep("", length(choix)))
  } else {
    names(select_choices) <- c(texte, names(choix))
  }

  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("select"),
      label = "",
      choices = select_choices,
      selected = "",
      multiple = choix_multiple
    )
  )
}

#' selecteur Server Functions
#'
#' @noRd
mod_selecteur_server <- function(id){
  moduleServer( id, function(input, output, session){
    reactive({
      input$select
    })
  })
}

## To be copied in the UI
# mod_selecteur_ui("selecteur_ui_1")

## To be copied in the server
# mod_selecteur_server("selecteur_ui_1")

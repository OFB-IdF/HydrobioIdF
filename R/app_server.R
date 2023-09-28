#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  mod_carte_server("carte", limites = limites_region)

  departements <- mod_selecteur_server(id = "departements")
  eqb <- mod_selecteur_server(id = "eqb")
  regie <- mod_checkbox_server(id = "regie")

}

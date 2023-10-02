#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  station <- mod_carte_server(
    "carte",
    stations = donnees_carte,
    departements = departements,
    eqb = eqb,
    suivi_regie = regie_seule
    )

  departements <- mod_selecteur_server(id = "departements")
  eqb <- mod_selecteur_server(id = "eqb")
  regie_seule <- mod_checkbox_server(id = "regie")

  mod_synthese_station_server(
    id = "synthese_station",
    choix_station = station,
    choix_eqb = eqb
    )

  mod_synthese_toutes_stations_server(
    id = "bilan_stations",
    choix_departement = departements,
    choix_eqb = eqb
  )
}

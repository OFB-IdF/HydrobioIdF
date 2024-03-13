#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Télécharge et charge dans l'espace de travail les données: "donnees_carte",
  # "donnees_carte_taxons", "indices", "listes_taxo", "resumes_listes",
  # "stations", "acronymes_indices", "date_donnees"
  mod_load_data_server("donnees")

  departements <- mod_selecteur_server(id = "departements")
  eqb <- mod_selecteur_server(id = "eqb")
  regie_seule <- mod_checkbox_server(id = "regie")

  station <- mod_carte_server(
    "carte",
    donnees_carte = donnees_carte,
    departements = departements,
    eqb = eqb,
    suivi_regie = regie_seule
    )

  ordre_taxon <- mod_selecteur_ordre_taxons_server(id = "ordre_taxons", choix_station = station, choix_eqb = eqb)

  mod_synthese_station_server(
    id = "synthese_station",
    resumes_listes, stations, indices, acronymes_indices, listes_taxo,
    choix_station = station,
    choix_eqb = eqb,
    ordre_taxon = ordre_taxon
    )

  mod_synthese_toutes_stations_server(
    id = "bilan_stations",
    stations, indices,
    choix_departement = departements,
    choix_eqb = eqb
  )

  taxon <- mod_repartition_taxons_server(
    id = "carte_taxons",
    listes = donnees_carte_taxons,
    departements = departements,
    eqb = eqb,
    suivi_regie = regie_seule
  )

  mod_synthese_taxon_server(
    id = "synthese_taxon",
    listes = listes_taxo,
    stations = stations,
    departements = departements,
    taxon = taxon,
    suivi_regie = regie_seule
  )
}

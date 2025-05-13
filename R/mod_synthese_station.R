#' synthese_station UI Function
#'
#' @description Module Shiny permettant d'afficher une synthèse des données pour une station spécifique.
#' Le module affiche les chiffres clés de la station, une chronique des indices biologiques,
#' et des graphiques de répartition des taxons.
#'
#' @param id Internal parameter for {shiny}.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return Un objet tagList contenant les éléments UI pour afficher la synthèse de la station.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_synthese_station_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("synthese_station"))
  )
}

#' synthese_station Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param resumes_listes Data frame contenant les résumés des listes faunistiques.
#' @param stations Data frame contenant les informations sur les stations.
#' @param regie Data frame contenant les informations sur le mode de gestion des stations.
#' @param indices Data frame contenant les valeurs des indices biologiques.
#' @param acronymes_indices Data frame contenant les acronymes des indices.
#' @param valeurs_seuils_stations Data frame contenant les seuils de qualité par station.
#' @param parametres_eqr Data frame contenant les paramètres pour le calcul des EQR.
#' @param etat_bio Data frame contenant les états biologiques.
#' @param listes_taxo Data frame contenant les listes taxonomiques.
#' @param choix_station Fonction réactive retournant le code de la station sélectionnée.
#' @param choix_eqb Fonction réactive retournant les codes des éléments de qualité sélectionnés.
#' @param ordre_taxon Fonction réactive retournant l'ordre d'affichage des taxons.
#' @param choix_stations Fonction réactive retournant la liste des stations sélectionnées.
#'
#' @details La fonction serveur gère l'affichage des différents composants de la synthèse :
#' - Les chiffres clés de la station (nombre de prélèvements, dates, etc.)
#' - La chronique des indices biologiques
#' - Les graphiques de répartition des taxons
#'
#' L'affichage est conditionnel à la sélection d'une station et d'éléments de qualité biologiques.
#'
#' @noRd
mod_synthese_station_server <- function(id, resumes_listes, stations, regie, indices, acronymes_indices, valeurs_seuils_stations, parametres_eqr, etat_bio, listes_taxo, choix_station, choix_eqb, ordre_taxon, choix_stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(choix_station(), choix_stations)

      station_regie <- stations %>%
        dplyr::filter(code_station_hydrobio == choix_station()) %>%
        dplyr::pull(regie)

      if (!choix_station() %in% choix_stations()) {

        output$synthese_station <- renderUI(NULL)

      } else {
        output$synthese_station <- renderUI({
          req(choix_station, choix_eqb)

          mod_chiffres_cles_station_server(
            id = "chiffres_cles",
            resumes_listes = resumes_listes,
            stations = stations,
            choix_station = choix_station,
            choix_eqb = choix_eqb
          )

          mod_chronique_indices_server(
            id = "chronique_indices",
            stations =stations,
            regie = regie,
            indices = indices,
            acronymes_indices = acronymes_indices,
            parametres_eqr = parametres_eqr,
            valeurs_seuils_stations = valeurs_seuils_stations,
            etat_bio = etat_bio,
            choix_station = choix_station,
            choix_eqb = choix_eqb
          )

          mod_chronique_taxons_server(
            id = "chronique_taxons",
            stations = stations,
            listes_taxo = listes_taxo,
            choix_station = choix_station,
            choix_eqb = choix_eqb,
            ordre_taxon = ordre_taxon
          )

          if (is.null(choix_station())) {
            div()
          } else {

            div(
              mod_chiffres_cles_station_ui(id = ns("chiffres_cles")),
              tags$div(
                class = "sub-tabpanel",
                tabsetPanel(
                  tabPanel(
                    title = "Indices",
                    mod_chronique_indices_ui(id = ns("chronique_indices"))
                  ),
                  tabPanel(
                    title = "Taxons",
                    mod_chronique_taxons_ui(id = ns("chronique_taxons"))
                  )
                )
              )
            )
          }

        })
      }
    })
  })
}

## To be copied in the UI
# mod_synthese_station_ui("synthese_station_1")

## To be copied in the server
# mod_synthese_station_server("synthese_station_1")

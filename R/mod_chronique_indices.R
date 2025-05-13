#' chronique_indices UI Function
#'
#' @description Module Shiny permettant d'afficher les chroniques des indices biologiques pour une station.
#' Le module génère des graphiques montrant l'évolution temporelle des indices biologiques
#' avec les seuils de qualité et les valeurs d'EQR associées.
#'
#' @param id Internal parameter for {shiny}.
#' @param stations Un objet sf contenant les stations hydrobiologiques.
#' @param regie Un data.frame indiquant les opérateurs des prélèvements.
#' @param indices Un data.frame contenant les indices biologiques.
#' @param acronymes_indices Un vecteur nommé des acronymes des indices.
#' @param valeurs_seuils_stations Un data.frame contenant les seuils de qualité par station.
#' @param parametres_eqr Un data.frame contenant les paramètres de calcul des EQR.
#' @param etat_bio Un data.frame contenant l'état biologique des stations.
#' @param choix_station Fonction réactive retournant le code de la station sélectionnée.
#' @param choix_eqb Fonction réactive retournant les codes des éléments de qualité sélectionnés.
#' @param input,output,session Internal parameters for {shiny}.
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
mod_chronique_indices_server <- function(id, stations, regie, indices, acronymes_indices, valeurs_seuils_stations, parametres_eqr, etat_bio, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    w <- 525

    observe({
      req(choix_station, choix_eqb)

      seuils_station <- valeurs_seuils_stations |>
        dplyr::filter(CODE_STATION %in% choix_station()) |>
        dplyr::mutate(libelle_indice = dplyr::case_when(
          indice == "IBG-DCE" ~ "IBG équivalent",
          indice == "MGCE" ~ "Invertébrés GCE",
          TRUE ~ indice
        )) |>
        dplyr::mutate(
          code_indice = names(acronymes_indices)[match(libelle_indice, acronymes_indices)]
        )

      parametres_eqr_station <- parametres_eqr |>
        purrr::map(
          function(df) {
            df |>
              dplyr::inner_join(
                seuils_station |>
                  dplyr::distinct(TYPO_NATIONALE, TG_BV)
              )
          }
        )

      DonneesGraphique <- filtrer_indices(stations, etat_bio, choix_station(), choix_eqb())

      output$indices <- renderPlot({
        tracer_chroniques_indices(
          DonneesGraphique = DonneesGraphique,
          acronymes_indices = acronymes_indices,
          regie = regie,
          seuils_station = seuils_station,
          parametres_eqr_station = parametres_eqr_station,
          interactive = FALSE
          )
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

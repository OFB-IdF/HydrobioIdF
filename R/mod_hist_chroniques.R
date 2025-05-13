#' hist_chroniques UI Function
#'
#' @description Module Shiny permettant d'afficher les chroniques temporelles des indices biologiques
#' sous forme de graphique interactif. Le module génère un graphique montrant l'évolution des valeurs
#' d'indices au cours du temps pour une ou plusieurs stations.
#'
#' @param id Internal parameter for {shiny}.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return Un objet tagList contenant un graphique plotly.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_hist_chroniques_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("chroniques"), height = "350px")
  )
}

#' hist_chroniques Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param donnees_graphique Un data.frame contenant les données à afficher avec les colonnes:
#'   - code_station_hydrobio: Code de la station
#'   - code_support: Code de l'élément de qualité biologique
#'   - date_prelevement: Date du prélèvement
#'   - resultat_indice: Valeur de l'indice
#'
#' @return Un objet plotly contenant le graphique des chroniques temporelles.
#'
#' @noRd
#' @importFrom plotly renderPlotly
mod_hist_chroniques_server <- function(id, donnees_graphique){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$chroniques <- plotly::renderPlotly({
      plot_chroniques(
        donnees_graphique,
        interactive = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_hist_chroniques_ui("hist_chroniques_1")

## To be copied in the server
# mod_hist_chroniques_server("hist_chroniques_1")

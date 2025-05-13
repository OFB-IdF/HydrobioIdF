#' hist_qualite UI Function
#'
#' @description Module Shiny permettant d'afficher l'historique des classes de qualité des indices biologiques
#' sous forme de graphique. Le module génère un graphique montrant l'évolution des classes de qualité
#' au cours du temps pour une ou plusieurs stations.
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
mod_hist_qualite_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("qualite"), height = "350px")
  )
}

#' hist_qualite Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param donnees_graphique Un data.frame contenant les données à afficher avec les colonnes:
#'   - code_station_hydrobio: Code de la station
#'   - code_support: Code de l'élément de qualité biologique
#'   - date_prelevement: Date du prélèvement
#'   - classe_etat: Classe de qualité de l'indice
#'
#' @return Un objet plotly contenant le graphique des classes de qualité.
#'
#' @noRd
#' @importFrom plotly renderPlotly
mod_hist_qualite_server <- function(id, donnees_graphique){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$qualite <- plotly::renderPlotly({
      plot_qualite(
        donnees_graphique,
        interactive = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_hist_qualite_ui("hist_qualite_1")

## To be copied in the server
# mod_hist_qualite_server("hist_qualite_1")

#' selecteur_ordre_taxons UI Function
#'
#' @description Module Shiny permettant de sélectionner l'ordre d'affichage des taxons dans les graphiques.
#' Le module propose différentes options de tri : ordre alphabétique, par abondance sur la chronique
#' complète, ou par abondance sur une année spécifique.
#'
#' @param id Internal parameter for {shiny}.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return Un objet tagList contenant deux éléments UI :
#'   - Un sélecteur pour le mode de tri des taxons
#'   - Un sélecteur d'année (affiché uniquement si le tri par année est sélectionné)
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selecteur_ordre_taxons_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("choix_ordre")),
    uiOutput(ns("choix_annee"))
  )
}

#' selecteur_ordre_taxons Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param choix_station Fonction réactive retournant le code de la station sélectionnée.
#' @param choix_eqb Fonction réactive retournant les codes des éléments de qualité sélectionnés.
#'
#' @details La fonction serveur gère l'affichage conditionnel du sélecteur d'année et retourne
#' une fonction réactive avec le mode de tri sélectionné. Les années disponibles sont déterminées
#' en fonction de la station et des éléments de qualité sélectionnés.
#'
#' @return Une fonction réactive retournant une liste avec :
#'   - ordre : Le mode de tri sélectionné
#'   - annee : L'année sélectionnée (si applicable)
#'
#' @noRd
mod_selecteur_ordre_taxons_server <- function(id, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$choix_ordre <- renderUI({
      if (is.null(choix_station())) {
        div()
      } else {
        tagList(
          h3("Taxons"),
          selectInput(
            inputId = ns("ordre"),
            label = "Mode de tri",
            choices = c(
              "ordre alphabétique",
              "abondance sur la chronique",
              "abondance sur une année"
            )
          )
        )
      }
    })


    annees_obs <- reactive({
      req(choix_station, choix_eqb)

      filtrer_listes(stations, listes_taxo, choix_station(), choix_eqb()) %>%
      dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
      dplyr::distinct(annee) %>%
      dplyr::arrange(dplyr::desc(annee)) %>%
      dplyr::pull(annee)
    })

    output$choix_annee <- renderUI({
      req(input$ordre)

     if (input$ordre == "abondance sur une année") {
        selectInput(
          label = "",
          inputId = ns("annee"),
          choices = annees_obs()
        )
      } else {
        div()
      }

    })

    reactive({
      req(input$ordre)

     if (input$ordre == "abondance sur une année") {
       req(input$annee)
        input$annee
      } else {
        input$ordre
      }
    })
  })
}

## To be copied in the UI
# mod_selecteur_ordre_taxons_ui("selecteur_ordre_taxons_1")

## To be copied in the server
# mod_selecteur_ordre_taxons_server("selecteur_ordre_taxons_1")

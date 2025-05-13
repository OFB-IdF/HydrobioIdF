#' chronique_taxons UI Function
#'
#' @description Module Shiny permettant d'afficher les chroniques des listes taxonomiques pour une station.
#' Le module génère des graphiques montrant l'évolution temporelle des taxons observés
#' pour chaque élément de qualité biologique (diatomées, invertébrés, macrophytes, poissons).
#'
#' @param id Internal parameter for {shiny}.
#' @param stations Un objet sf contenant les stations hydrobiologiques.
#' @param listes_taxo Un data.frame contenant les listes taxonomiques.
#' @param choix_station Fonction réactive retournant le code de la station sélectionnée.
#' @param choix_eqb Fonction réactive retournant les codes des éléments de qualité sélectionnés.
#' @param ordre_taxon Fonction réactive retournant l'ordre de tri des taxons.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chronique_taxons_ui <- function(id){
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
      uiOutput(ns("tabs"))


  )
}

#' chronique_taxons Server Functions
#'
#' @noRd
mod_chronique_taxons_server <- function(id, stations, listes_taxo, choix_station, choix_eqb, ordre_taxon){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    w <- 525

    observe({
      req(choix_station, choix_eqb)

      liste_station <- filtrer_listes(stations, listes_taxo, choix_station(), choix_eqb())
      liste_diato <- liste_station %>%
          dplyr::filter(code_support == 10)
      liste_inv <- liste_station %>%
        dplyr::filter(code_support == 13)
      liste_macro <- liste_station %>%
        dplyr::filter(code_support == 27)
      liste_pois <- liste_station %>%
        dplyr::filter(code_support == 4)

      observe({
        req(ordre_taxon())

        output$diatomees <- renderPlot({
          tracer_chroniques_taxons(
            liste_station = liste_diato,
            ordre_taxon = ordre_taxon()
            )
        },
        height = dplyr::n_distinct(liste_diato$code_appel_taxon) * 20 + 20,
        width = w
        )
        output$invertebres <- renderPlot({
          tracer_chroniques_taxons(
            liste_station = liste_inv,
            ordre_taxon = ordre_taxon()
            )
        },
        height = dplyr::n_distinct(liste_inv$code_appel_taxon) * 20 + 20,
        width = w
        )
        output$macrophytes <- renderPlot({
          tracer_chroniques_taxons(
            liste_station = liste_macro,
            ordre_taxon = ordre_taxon()
            )
        },
        height = dplyr::n_distinct(liste_macro$code_appel_taxon) * 20 + 20,
        width = w
        )
        output$poissons <- renderPlot({
          tracer_chroniques_taxons(
            liste_station = liste_pois,
            ordre_taxon = ordre_taxon()
            )
        },
        height = dplyr::n_distinct(liste_pois$code_appel_taxon) * 20 + 20,
        width = w
        )

      })

      output$tabs <- renderUI({
        h <- 525
        w <- 550

        if (nrow(liste_diato) > 0) {
          tab10 <- tabPanel(
            title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Diatomées</p>"),
            shinydashboard::box(
              plotOutput(outputId = ns("diatomees"),
                         height = h)
            )
          )
        } else {
          tab10 <- NULL
        }

        if (nrow(liste_macro) > 0) {
          tab27 <- tabPanel(
            title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Macrophytes</p>"),
            shinydashboard::box(
              plotOutput(outputId = ns("macrophytes"),
                         height = h)
            )
          )
        } else {
          tab27 <- NULL
        }

        if (nrow(liste_inv) > 0) {
          tab13 <- tabPanel(
            title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Macroinvertébrés</p>"),
            shinydashboard::box(
              plotOutput(outputId = ns("invertebres"),
                         height = h)
            )
          )
        } else {
          tab13 <- NULL
        }

        if (nrow(liste_pois) > 0) {
          tab4 <- tabPanel(
            title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Poissons</p>"),
            shinydashboard::box(
              plotOutput(outputId = ns("poissons"),
                         height = h)
            )
          )
        } else {
          tab4 <- NULL
        }


        tags$div(
          class = "sub-tabpanel",
          tabsetPanel(
            tab10,
            tab27,
            tab13,
            tab4
          )
        )
      })
    })
  })
}

## To be copied in the UI
# mod_chronique_taxons_ui("chronique_taxons_1")

## To be copied in the server
# mod_chronique_taxons_server("chronique_taxons_1")

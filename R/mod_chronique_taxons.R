#' chronique_taxons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chronique_taxons_ui <- function(id){
  ns <- NS(id)
  h <- 500

  css <- HTML(
    paste0(".box-body {
               max-height: ", h, "px !important;
               min-width: 450px !important;
               overflow-y: scroll;
           }")
  )

    tagList(
      tags$head(
        tags$style(css)
      ),

    tags$div(
      class = "sub-tabpanel",
      tabsetPanel(
        tabPanel(
          title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Diatomées</p>"),
          shinydashboard::box(
            plotOutput(outputId = ns("diatomees"),
                       height = h)
            )
        ),
        tabPanel(
          title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Macrophytes</p>"),
          shinydashboard::box(
            plotOutput(outputId = ns("macrophytes"),
                       height = h)
          )
        ),
        tabPanel(
          title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Macroinvertébrés</p>"),
          shinydashboard::box(
            plotOutput(outputId = ns("invertebres"),
                       height = h)
          )
        ),
        tabPanel(
          title = HTML("<p style='font-size:14px;color:black;margin:0px;'>Poissons</p>"),
          shinydashboard::box(
            plotOutput(outputId = ns("poissons"),
                       height = h)
          )
        )
      )
    )
  )
}

#' chronique_taxons Server Functions
#'
#' @noRd
mod_chronique_taxons_server <- function(id, stations, listes_taxo, choix_station, choix_eqb, ordre_taxon){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(choix_station, choix_eqb, ordre_taxon())

      liste_station <- filtrer_listes(stations, listes_taxo, choix_station(), choix_eqb())
      liste_diato <- liste_station %>%
          dplyr::filter(code_support == 10)
      liste_inv <- liste_station %>%
        dplyr::filter(code_support == 13)
      liste_macro <- liste_station %>%
        dplyr::filter(code_support == 27)
      liste_pois <- liste_station %>%
        dplyr::filter(code_support == 4)

      output$diatomees <- renderPlot({
          tracer_chroniques_taxons(liste_diato, ordre_taxon())
      },
      height = dplyr::n_distinct(liste_diato$code_appel_taxon) * 20 + 20
      )
      output$invertebres <- renderPlot({
        tracer_chroniques_taxons(liste_inv, ordre_taxon())
      },
      height = dplyr::n_distinct(liste_inv$code_appel_taxon) * 20 + 20
      )
      output$macrophytes <- renderPlot({
        tracer_chroniques_taxons(liste_macro, ordre_taxon())
      },
      height = dplyr::n_distinct(liste_macro$code_appel_taxon) * 20 + 20
      )
      output$poissons <- renderPlot({
        tracer_chroniques_taxons(liste_pois, ordre_taxon())
      },
      height = dplyr::n_distinct(liste_pois$code_appel_taxon) * 20 + 20
      )

    })
  })
}

## To be copied in the UI
# mod_chronique_taxons_ui("chronique_taxons_1")

## To be copied in the server
# mod_chronique_taxons_server("chronique_taxons_1")

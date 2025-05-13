#' synthese_taxon UI Function
#'
#' @description Module Shiny permettant d'afficher une synthèse des données pour un taxon spécifique.
#' Le module affiche des graphiques montrant l'évolution temporelle des abondances du taxon
#' sur l'ensemble des stations sélectionnées.
#'
#' @param id Internal parameter for {shiny}.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return Un objet tagList contenant deux graphiques :
#' - Un graphique montrant la présence du taxon par station au fil du temps
#' - Un graphique montrant l'évolution des abondances du taxon
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_synthese_taxon_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("chronique_stations")),
    plotOutput(ns("chronique_abondances"))
  )
}

#' synthese_taxon Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param repartition Liste réactive contenant les informations sur le taxon sélectionné.
#' @param choix_stations Fonction réactive retournant la liste des stations sélectionnées.
#'
#' @details La fonction serveur gère la création de deux graphiques :
#' - Un graphique temporel montrant sur quelles stations le taxon a été observé
#' - Un graphique temporel montrant l'évolution des abondances du taxon
#'
#' Les graphiques sont mis à jour automatiquement en fonction de la sélection des stations
#' et du taxon choisi dans l'interface.
#'
#' @noRd
mod_synthese_taxon_server <- function(id, repartition, choix_stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(choix_stations)

      if (!is.null(repartition$taxon)) {
        listes_dep <- listes_taxo %>%
          dplyr::filter(
            code_station_hydrobio %in% choix_stations()
          ) %>%
          dplyr::mutate(annee = lubridate::year(date_prelevement))

        listes_taxon <- listes_dep %>%
          dplyr::filter(
            libelle_taxon == repartition$taxon
          )

        if (nrow(listes_taxon) > 0) {
          x_breaks <- integer_breaks(n = 3)(listes_taxon$annee)

          output$chronique_stations <- renderPlot({
            listes_dep %>%
              dplyr::filter(
                annee >= min(listes_taxon$annee) & annee <= max(listes_taxon$annee),
                libelle_support == unique(listes_taxon$libelle_support)
              ) %>%
              dplyr::group_by(annee, libelle_station_hydrobio) %>%
              dplyr::summarise(
                esp_pres = any(libelle_taxon %in% repartition$taxon),
                .groups = "drop"
              ) %>%
              dplyr::group_by(annee) %>%
              dplyr::mutate(
                n_sta_tot = dplyr::n_distinct(libelle_station_hydrobio)
              ) %>%
              dplyr::ungroup() %>%
              dplyr::group_by(annee, esp_pres) %>%
              dplyr::summarise(
                n_sta = dplyr::n_distinct(libelle_station_hydrobio),
                n_sta_tot = unique(n_sta_tot),
                .groups = "drop"
              ) %>%
              tidyr::complete(
                tidyr::nesting(annee, n_sta_tot), esp_pres,
                fill = list(n_sta = 0)
              ) %>%
              dplyr::filter(!(!esp_pres & n_sta == 0)) %>%
              dplyr::mutate(
                y_label = ifelse(esp_pres, n_sta, n_sta_tot)
              ) %>%
              ggplot2::ggplot() +
              ggplot2::geom_col(
                mapping = ggplot2::aes(
                  x = annee,
                  y = n_sta,
                  fill = esp_pres
                ),
                width = .95,
                colour = c("black")
              ) +
              # ggplot2::geom_text(
              #   mapping = ggplot2::aes(
              #     x = annee,
              #     y = y_label,
              #     label = n_sta
              #   ),
              #   vjust = -.25
              # ) +
              ggplot2::labs(
                x = "",
                y = "Nombre de stations"
              ) +
              ggplot2::scale_x_continuous(
                breaks = x_breaks
              ) +
              ggplot2::scale_fill_manual(
                values = c(
                  `TRUE` = "white",
                  `FALSE` = "darkgrey"
                )
              ) +
              ggplot2::theme_minimal(base_size = 16) +
              ggplot2::theme(
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.x = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(hjust = .95),
                legend.position = "none"
              )
          })

          output$chronique_abondances <- renderPlot({
            plot_ab <- listes_taxon %>%
              dplyr::group_by(libelle_taxon, annee) %>%
              dplyr::summarise(ab_moy = sum(resultat_taxon) / dplyr::n_distinct(code_station_hydrobio)) %>%
              ggplot2::ggplot() +
              ggplot2::geom_col(
                mapping = ggplot2::aes(
                  x = annee,
                  y = ab_moy
                ),
                fill = "white", colour = "black",
                width = .95
              ) +
              ggplot2::labs(
                x = "",
                y = "Abondance moyenne par station"
              ) +
              ggplot2::scale_x_continuous(
                breaks = x_breaks
              ) +
              ggplot2::theme_minimal(base_size = 16) +
              ggplot2::theme(
                plot.subtitle = ggplot2::element_text(
                  colour = "#6495ED",
                  face = "bold"
                    ),
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.x = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(hjust = .95),
                legend.position = "right"
              )

            if (!is.null(repartition$station)) {
              plot_ab <- plot_ab +
                ggplot2::geom_point(
                  data = listes_taxon %>%
                    dplyr::filter(code_station_hydrobio == repartition$station) %>%
                    dplyr::group_by(annee) %>%
                    dplyr::summarise(ab_moy = mean(resultat_taxon)),
                  mapping = ggplot2::aes(
                    x = annee, y = ab_moy
                  ),
                  shape = 21, size = 6,
                  fill = "#6495ED", colour = "black"
                ) +
                ggplot2::labs(subtitle = stations %>%
                                dplyr::filter(code_station_hydrobio == repartition$station) %>%
                                dplyr::pull(libelle_station_hydrobio))
            }

            plot_ab
          })

        } else {
          output$chronique_stations <- renderPlot(NULL)
          output$chronique_abondances <- renderPlot(NULL)
        }
      }
    })
  })
}

## To be copied in the UI
# mod_synthese_taxon_ui("synthese_taxon_1")

## To be copied in the server
# mod_synthese_taxon_server("synthese_taxon_1")

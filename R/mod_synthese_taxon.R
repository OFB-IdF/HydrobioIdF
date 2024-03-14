#' synthese_taxon UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
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
#' @noRd
mod_synthese_taxon_server <- function(id, listes, stations, departements, taxon, suivi_regie){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        if (length(unique(na.omit(x))) == 1) {
          breaks <- unique(na.omit(x))
        } else {
          breaks <- floor(pretty(x, n, ...))
        }

        names(breaks) <- attr(breaks, "labels")
        unique(breaks)
      }
      return(fxn)
    }

    listes <- listes %>%
            dplyr::left_join(
              stations %>%
                dplyr::select(code_station_hydrobio, code_departement),
              by = c("code_station_hydrobio"), multiple = "all"
            )


    observe({
      req(departements, taxon, suivi_regie)

      deps <- departements()
      if (is.null(deps))
        deps <- unique(listes$code_departement)
      if ("PPC" %in% deps)
        deps <- c(deps[deps != "PPC"], 75, 92, 93, 94)

      # if (suivi_regie())
      #   DonneesCarte <- DonneesCarte %>%
      #   dplyr::filter(regie & choix_eqb != 4)

      listes_dep <- listes %>%
        dplyr::filter(
          code_departement %in% deps
        ) %>%
        dplyr::mutate(annee = lubridate::year(date_prelevement))

      listes_taxon <- listes_dep %>%
        dplyr::filter(
          libelle_taxon == taxon()
        )

      if (nrow(listes_taxon) > 0) {
        x_breaks <- integer_breaks(n = 3)(listes_taxon$annee)

              output$chronique_abondances <- renderPlot({
        listes_taxon %>%
          dplyr::group_by(libelle_taxon, annee) %>%
          dplyr::summarise(ab_moy = sum(resultat_taxon) / dplyr::n_distinct(code_station_hydrobio)) %>%
          ggplot2::ggplot() +
          ggplot2::geom_col(
            mapping = ggplot2::aes(
              x = annee,
              y = ab_moy
            ),
            fill = "#FFB90F",
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
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(hjust = .95),
            legend.position = "right"
          )
      })

              output$chronique_stations <- renderPlot({
                listes_dep %>%
                  dplyr::filter(
                    annee >= min(listes_taxon$annee) & annee <= max(listes_taxon$annee),
                    libelle_support == unique(listes_taxon$libelle_support)
                  ) %>%
                  dplyr::group_by(annee, libelle_station_hydrobio) %>%
                  dplyr::summarise(
                    esp_pres = any(libelle_taxon %in% taxon()),
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
                    y_label = ifelse(esp_pres, n_sta + 3, n_sta_tot + 3)
                  ) %>%
                  ggplot2::ggplot() +
                  ggplot2::geom_col(
                    mapping = ggplot2::aes(
                      x = annee,
                      y = n_sta,
                      fill = esp_pres
                    ),
                    width = .95,
                    colour = c("#104E8B")
                  ) +
                  ggplot2::geom_text(
                    mapping = ggplot2::aes(
                      x = annee,
                      y = y_label,
                      label = n_sta
                    )
                  ) +
                  ggplot2::labs(
                    x = "",
                    y = "Nombre de stations"
                  ) +
                  ggplot2::scale_x_continuous(
                    breaks = x_breaks
                  ) +
                  ggplot2::scale_fill_manual(
                    values = c(
                      `TRUE` = "#6495ED",
                      `FALSE` = "white"
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
      }

    })
  })
}

## To be copied in the UI
# mod_synthese_taxon_ui("synthese_taxon_1")

## To be copied in the server
# mod_synthese_taxon_server("synthese_taxon_1")

#' Title
#'
#' @param DonneesGraphique
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr mutate left_join select group_by group_split
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point scale_colour_manual theme_light theme element_blank labs facet_wrap vars scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom patchwork wrap_plots
#' @importFrom purrr map
#' @importFrom stringr str_replace_na
tracer_chroniques_indices <- function(DonneesGraphique, acronymes_indices, regie, seuils_station, interactive = FALSE) {

  x_lims <- range(na.omit(DonneesGraphique$annee))
  if (length(unique(x_lims)) == 1)
    x_lims <- x_lims + c(-1, +1)

  x_breaks <- integer_breaks(n = 3)(DonneesGraphique$annee)

  graphique <- DonneesGraphique %>%
    dplyr::mutate(
      libelle_indice = acronymes_indices[as.character(code_indice)] %>%
        factor(levels = acronymes_indices)
    ) %>%
    dplyr::left_join(
      regie %>%
        dplyr::mutate(regie = realisation == 1) %>%
        dplyr::select(
          code_station_hydrobio = code_station,
          code_indice, annee, regie
        ),
      by = c("code_station_hydrobio", "code_indice", "annee"),
      multiple = "all"
    ) %>%
    dplyr::mutate(
      regie = ifelse(is.na(regie), FALSE, regie),
      type_resultat = ifelse(!is.na(eqr_indice), "eqr", "brut"),
      resultat_indice = ifelse(!is.na(eqr_indice), eqr_indice, resultat_indice)
      ) %>%
    dplyr::group_by(libelle_indice) %>%
    dplyr::group_split() %>%
    purrr::map(
      function(data_indice) {
        p <- data_indice %>%
          ggplot2::ggplot() +
          ggplot2::geom_rect(
            data = seuils_station |>
              dplyr::filter(libelle_indice %in% data_indice$libelle_indice),
            mapping = ggplot2::aes(
              ymin = seuil_bas, ymax = seuil_haut,
              fill = classe
            ),
            xmin = x_lims[1], xmax = x_lims[2],
            alpha = .25
          ) +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              x = annee,
              y = resultat_indice,
              colour = regie
            ),
            size = 2
          ) +
          ggplot2::geom_smooth(
            mapping = ggplot2::aes(
              x = annee,
              y = resultat_indice
              ),
            se = FALSE,
            method = "gam",
            formula = y ~ s(x, k = 2),
            linetype = "dotted",
            linewidth = .75,
            colour = "black"
          ) +
          ggplot2::scale_colour_manual(
            guide = "none",
            values = c(
              `TRUE` = "blue",
              `FALSE` = "grey40"
            )
          ) +
          ggplot2::scale_fill_manual(
            guide = "none",
            values = c(
              MAUVAIS = "#EE2C2C",
              MEDIOCRE = "#FF7F00",
              MOYEN = "#FFFF00",
              BON = "#00FF00",
              TRES_BON = "#0000EE"
              )
          ) +
          ggplot2::theme_light(base_size = 14) +
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
              linetype = "dotted"
            )
          ) +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::facet_wrap(ggplot2::vars(libelle_indice)) +
          ggplot2::scale_x_continuous(
            limits = x_lims,
            breaks = x_breaks
          )

        if (unique(data_indice$code_indice) %in%
            c(5856, 2928, 5910, 6951) && unique(data_indice$type_resultat) == "brut")
          p <- p +
            ggplot2::scale_y_continuous(
              limits = c(0, 20),
              breaks = c(0, 5, 10, 15, 20)
            )

        if (unique(data_indice$code_indice) %in% c(5856, 2928, 5910, 6951) && unique(data_indice$type_resultat) == "eqr")
          p <- p +
            ggplot2::scale_y_continuous(
              limits = c(0, 1),
              breaks = c(0, .25, .5, .75, 1)
            )

        if (unique(data_indice$code_indice) %in% c(7613))
          p <- p +
            ggplot2::scale_y_continuous(
              limits = c(0, 1),
              breaks = c(0, .25, .5, .75, 1)
            )

        if (unique(data_indice$code_indice) %in% c(7036))
          p <- p +
            ggplot2::scale_y_reverse(
              limits = c(60, 0),
              breaks = c(60, 45, 30, 15, 0)
            )

        p
      }
    ) %>%
    patchwork::wrap_plots(ncol = 1, byrow = TRUE)

  if (interactive) {
    plotly::ggplotly(graphique)
  } else {
    graphique
  }
}

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
tracer_chroniques_indices <- function(DonneesGraphique, acronymes_indices, interactive = FALSE) {
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


  x_lims <- range(na.omit(DonneesGraphique$annee))
  x_breaks <- integer_breaks(n = 3)(DonneesGraphique$annee)

  graphique <- DonneesGraphique %>%
    dplyr::mutate(
      libelle_indice = acronymes_indices[as.character(code_indice)] %>%
        factor(levels = acronymes_indices)
    ) %>%
    dplyr::left_join(
      regie %>%
        dplyr::select(
          code_station_hydrobio = code_station,
          code_indice, annee
        ) %>%
        dplyr::mutate(regie = TRUE),
      by = c("code_station_hydrobio", "code_indice", "annee"),
      multiple = "all"
    ) %>%
    dplyr::mutate(regie = ifelse(is.na(regie), FALSE, regie)) %>%
    dplyr::group_by(libelle_indice) %>%
    dplyr::group_split() %>%
    purrr::map(
      function(data_indice) {
        p <- data_indice %>%
          ggplot2::ggplot(
            mapping = ggplot2::aes(
              x = annee,
              y = resultat_indice
            )
          ) +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              colour = regie
            ),
            size = 2
          ) +
          ggplot2::geom_smooth(
            se = FALSE,
            method = "gam",
            formula = y ~ s(x, k = 2),
            linetype = "dotted",
            size = .75,
            colour = "black"
          ) +
          ggplot2::scale_colour_manual(
            guide = "none",
            values = c(
              `TRUE` = "blue",
              `FALSE` = "darkgrey"
            )
          ) +
          ggplot2::theme_light(base_size = 14) +
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()
          ) +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::facet_wrap(ggplot2::vars(libelle_indice)) +
          ggplot2::scale_x_continuous(
            limits = x_lims,
            breaks = x_breaks
          )

        if (unique(data_indice$code_indice) %in%
            c(5856, 2928, 5910, 6951))
          p <- p +
            ggplot2::scale_y_continuous(
              limits = c(0, 20),
              breaks = c(0, 5, 10, 15, 20)
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

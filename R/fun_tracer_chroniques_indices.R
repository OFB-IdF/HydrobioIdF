#' Tracer les chroniques des indices biologiques
#'
#' @param DonneesGraphique Un data.frame contenant les indices biologiques avec les colonnes
#'   annee, code_indice, libelle_indice, resultat_indice et classe_indice
#' @param acronymes_indices Un vecteur nommé des acronymes des indices
#' @param regie Un data.frame contenant les informations de suivi en régie
#' @param seuils_station Un data.frame contenant les seuils de qualité pour la station
#' @param parametres_eqr_station Un data.frame contenant les paramètres de calcul des EQR
#' @param interactive Logique. Si TRUE, produit des graphiques interactifs avec plotly
#'
#' @return Une liste d'objets ggplot2 ou plotly (si interactive = TRUE) représentant
#'   les chroniques des indices biologiques avec les seuils de qualité et les EQR
#' @export
#'
#' @details Cette fonction trace les chroniques des indices biologiques en représentant
#'   à la fois les valeurs brutes et les EQR (Ecological Quality Ratio). Les graphiques
#'   incluent :
#'   \itemize{
#'     \item Les valeurs des indices avec les seuils de qualité
#'     \item Les valeurs d'EQR avec les seuils normalisés
#'     \item Une courbe de tendance (loess)
#'     \item Un code couleur selon la classe de qualité
#'   }
#'
#' @examples
#' \dontrun{
#' chroniques <- tracer_chroniques_indices(
#'   DonneesGraphique = indices_station,
#'   acronymes_indices = c("5856" = "IBD", "2928" = "IBMR"),
#'   regie = suivi_regie,
#'   seuils_station = seuils,
#'   parametres_eqr_station = params_eqr
#' )
#' }
#' @importFrom dplyr mutate left_join select group_by group_split
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point scale_colour_manual theme_light theme element_blank labs facet_wrap vars scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom patchwork wrap_plots
#' @importFrom purrr map
#' @importFrom stringr str_replace_na
tracer_chroniques_indices <- function(DonneesGraphique, acronymes_indices, regie, seuils_station, parametres_eqr_station, interactive = FALSE) {

  x_lims <- range(na.omit(DonneesGraphique$annee))
  if (length(unique(x_lims)) == 1)
    x_lims <- x_lims + c(-1, +1)

  x_breaks <- integer_breaks(n = 3)(DonneesGraphique$annee)

  conversion_eqr <- function(valeurs, code_indice, parametres_eqr_station) {
    if (code_indice == 5856) {
      eqr <- (valeurs - parametres_eqr_station[[code_indice]]$MINIMUM) / (parametres_eqr_station[[code_indice]]$REFERENCE - parametres_eqr_station[[code_indice]]$MINIMUM)
    }

    if (code_indice == 2928) {
      eqr <- valeurs / parametres_eqr_station[[code_indice]]$REFERENCE
    }

    if (code_indice %in% c(6951, 5910)) {
      eqr <- (valeurs - 1) / (parametres_eqr_station[[code_indice]]$REFERENCE - 1)
    }

    eqr
  }

  retroconversion_eqr <- function(valeurs_eqr, code_indice, parametres_eqr_station) {
    val <- valeurs_eqr

    if (code_indice == 5856) {
      val <- valeurs_eqr * (parametres_eqr_station[[code_indice]]$REFERENCE - parametres_eqr_station[[code_indice]]$MINIMUM) + parametres_eqr_station[[code_indice]]$MINIMUM
    }

    if (code_indice == 2928) {
      val <- valeurs_eqr * parametres_eqr_station[[code_indice]]$REFERENCE
    }

    if (code_indice %in% c(6951, 5910)) {
      val <- valeurs_eqr * (parametres_eqr_station[[code_indice]]$REFERENCE - 1) + 1
    }

    val
  }

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
      eqr = !is.na(eqr_indice)
      ) %>%
    dplyr::group_by(libelle_indice) %>%
    dplyr::group_split() %>%
    purrr::map(
      function(data_indice) {
        p <- data_indice %>%
          ggplot2::ggplot() +
          ggplot2::geom_rect(
            data = seuils_station |>
              dplyr::filter(
                libelle_indice %in% data_indice$libelle_indice
                ) |>
              dplyr::mutate(
                seuil_bas_raw = ifelse(
                  test = code_indice %in% c(5856, 2928, 5910, 6951) & !is.na(seuil_bas),
                  yes = retroconversion_eqr(
                    valeurs_eqr = seuil_bas,
                    code_indice = unique(data_indice$code_indice),
                    parametres_eqr_station = parametres_eqr_station
                  ),
                  no = seuil_bas
                ) ,
                seuil_haut_raw = dplyr::case_when(
                  code_indice %in% c(5856, 2928, 5910, 6951) &
                    !is.na(seuil_haut) &
                    classe == "TRES_BON" ~ 20,
                  code_indice %in% c(5856, 2928, 5910, 6951) &
                    !is.na(seuil_haut) ~
                    retroconversion_eqr(
                      valeurs_eqr = seuil_haut,
                      code_indice = unique(data_indice$code_indice),
                      parametres_eqr_station = parametres_eqr_station
                    ),
                  TRUE ~ seuil_haut
                )
              ),
            mapping = ggplot2::aes(
              ymin = seuil_bas_raw,
              ymax = seuil_haut_raw,
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
            c(5856, 2928, 5910, 6951) && !all(data_indice$eqr))
          p <- p +
            ggplot2::scale_y_continuous(
              limits = c(0, 20),
              breaks = c(0, 5, 10, 15, 20)
            )

        if (unique(data_indice$code_indice) %in% c(5856, 2928, 5910, 6951) && all(data_indice$eqr))
          p <- p +
            ggplot2::scale_y_continuous(
              limits = c(0, 20),
              breaks = c(0, 5, 10, 15, 20),
              sec.axis = ggplot2::sec_axis(
                name = "EQR",
                transform = ~ conversion_eqr(
                  valeurs = .,
                  code_indice = unique(data_indice$code_indice),
                  parametres_eqr_station = parametres_eqr_station
                  ),
                breaks = c(0, .25, .5, .75, 1)
                )
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


#' Tracer un graphique de l'évolution de la qualité
#'
#' @param donnees_graphique Un data.frame ou objet sf contenant les données de qualité avec
#'   les colonnes code_station_hydrobio, libelle_support, libelle_indice, classe_indice et annee
#' @param interactive Logique. Si TRUE, produit un graphique interactif avec plotly
#'
#' @return Un objet ggplot2 ou plotly (si interactive = TRUE) représentant l'évolution
#'   de la qualité par indice biologique et par cycle DCE
#' @export
#'
#' @details Cette fonction produit un graphique en barres empilées montrant l'évolution de
#'   la qualité écologique par indice biologique. Les données sont regroupées par cycle DCE
#'   (avant 2010, 2010-2015, 2016-2021, 2022-2027) et les classes de qualité sont
#'   représentées selon le code couleur DCE (bleu, vert, jaune, orange, rouge).
#'
#' @examples
#' \dontrun{
#' plot_qualite(indices)
#' plot_qualite(indices, interactive = TRUE)
#' }
#'
#' @importFrom dplyr as_tibble mutate distinct count case_when
#' @importFrom ggplot2 ggplot geom_col aes position_dodge scale_fill_manual labs theme_minimal theme element_blank element_text
#' @importFrom plotly ggplotly layout add_annotations
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_split
plot_qualite <- function(donnees_graphique, interactive = FALSE) {
  if ("sf" %in% class(donnees_graphique))
    donnees_graphique <- donnees_graphique %>%
      sf::st_drop_geometry() %>%
      dplyr::as_tibble()

  PlotQualite <- donnees_graphique %>%
    dplyr::mutate(
      libelle_support = libelle_support %>%
        stringr::str_split(pattern = " ") %>%
        sapply(FUN = function(x) {x[1]}) %>%
        factor(levels = c("Diatomées", "Macrophytes", "Macroinvertébrés", "Poissons")),
      libelle_indice = libelle_indice |>
        factor(levels = c("IBD", "IBMR", "IPR", "I2M2", "IBG équivalent", "Invertébrés GCE"))
      ) %>%
    dplyr::distinct(code_station_hydrobio, annee, libelle_support, libelle_indice, classe_indice) %>%
    dplyr::mutate(
      classe_indice = stringr::str_replace_na(classe_indice, "Non évalué") |>
        factor(levels = c("Non évalué", "TRES_BON", "BON", "MOYEN", "MEDIOCRE", "MAUVAIS")),
      cycle_dce = dplyr::case_when(
        annee < 2010 ~ "< 2010",
        annee < 2016 ~ "2010-2015",
        annee < 2022 ~ "2016-2021",
        annee < 2028 ~ "2022-2027"
      ) |>
        factor(levels = c("< 2010", "2010-2015", "2016-2021", "2022-2027"))
    ) |>
    dplyr::group_by(cycle_dce, libelle_support, libelle_indice, classe_indice) |>
    dplyr::summarise(n = dplyr::n_distinct(code_station_hydrobio), .groups = "drop") |>
    ggplot2::ggplot() +
    ggplot2::geom_col(
      mapping = ggplot2::aes(x = cycle_dce, y = n, fill = classe_indice),
      position = ggplot2::position_stack(), alpha = .5
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c(
        MAUVAIS = "#EE2C2C",
        MEDIOCRE = "#FF7F00",
        MOYEN = "#FFFF00",
        BON = "#00FF00",
        TRES_BON = "#0000EE",
        `Non évalué` = "grey"
      )
    ) +
    ggplot2::labs(
      x = "",
      y = "Nombre de station"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(hjust = .95),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(libelle_indice), scales = "free_y")

  if (interactive) {
    (PlotQualite +
      ggplot2::labs(y = "")) |>
      plotly::ggplotly() %>%
      plotly::layout(
        showlegend = FALSE,
        legend = list(
          orientation = 'v',
          title = list(
            side = 'top'
          )
        ),
        xaxis = list(title = FALSE, tickangle = 45, tickalign = "top"),
        yaxis = list(title = FALSE),
        margin = list(
          t = 50, b = 100, r = 50, l = 100
        )
      ) %>%
      plotly::add_annotations(
        xref = "paper", yref = "paper",
        x = 1, y = -.175,
        text = "Cycle DCE",
        showarrow = FALSE
      ) %>%
      plotly::add_annotations(
        xref = "paper", yref = "paper",
        x = -.2, y = 1,
        text = "Nombre de stations",
        showarrow = FALSE,
        textangle = -90
      )
  } else {
    PlotQualite
  }
}


#' Tracer un graphique des chroniques de suivi
#'
#' @param donnees_graphique Un data.frame ou objet sf contenant les données de suivi avec
#'   les colonnes code_station_hydrobio, libelle_support et annee
#' @param interactive Logique. Si TRUE, produit un graphique interactif avec plotly
#'
#' @return Un objet ggplot2 ou plotly (si interactive = TRUE) représentant la distribution
#'   du nombre d'années de suivi par élément de qualité biologique
#' @export
#'
#' @details Cette fonction produit un graphique en barres montrant la distribution du nombre
#'   d'années de suivi par élément de qualité biologique. Les chroniques sont regroupées en
#'   4 classes : 1 an, 2-5 ans, 6-10 ans et plus de 10 ans.
#'
#' @examples
#' \dontrun{
#' plot_chroniques(indices)
#' plot_chroniques(indices, interactive = TRUE)
#' }
#'
#' @importFrom dplyr as_tibble mutate distinct count case_when
#' @importFrom ggplot2 ggplot geom_col aes position_dodge scale_fill_manual labs theme_minimal theme element_blank element_text
#' @importFrom plotly ggplotly layout add_annotations
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_split
plot_chroniques <- function(donnees_graphique, interactive = FALSE) {
  if ("sf" %in% class(donnees_graphique))
    donnees_graphique <- donnees_graphique %>%
      sf::st_drop_geometry() %>%
      dplyr::as_tibble()

  PlotChroniques <- donnees_graphique %>%
    dplyr::mutate(libelle_support = libelle_support %>%
                    stringr::str_split(pattern = " ") %>%
                    sapply(FUN = function(x) {x[1]}) %>%
                    factor(levels = c("Diatomées", "Macrophytes", "Macroinvertébrés", "Poissons"))) %>%
    dplyr::distinct(code_station_hydrobio, libelle_support, annee) %>%
    dplyr::count(code_station_hydrobio, libelle_support) %>%
    dplyr::mutate(
      Chronique = dplyr::case_when(
        n == 1 ~ "1",
        n <= 5 ~ "2-5",
        n <= 10 ~ "6-10",
        n > 10 ~ "> 10 ans"
      ) %>%
        factor(levels = c("1", "2-5", "6-10", "> 10 ans"))
    ) %>%
    dplyr::count(libelle_support, Chronique) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
      mapping = ggplot2::aes(x = Chronique, y = n, fill = libelle_support),
      position = ggplot2::position_dodge(preserve = "single")
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c(
        "Poissons" = "#8DB6CD",
        "Macroinvertébrés" = "#CD6600",
        "Macrophytes" = "#228B22",
        "Diatomées" = "#B3EE3A"
      )
    ) +
    ggplot2::labs(
      x = "Années de suivi",
      y = "Nombre de station",
      fill = "\nElément de qualité\nbiologique"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(hjust = .95),
      legend.position = "right"
    )

  if (interactive) {
    plotly::ggplotly(PlotChroniques) %>%
      plotly::layout(
        legend = list(
          orientation = 'v',
          title = list(
            side = 'top'
          )
        ),
        xaxis = list(title = FALSE),
        yaxis = list(title = FALSE),
        margin = list(
          t = 50, b = 100, r = 50, l = 100
        )
      ) %>%
      plotly::add_annotations(
        xref = "paper", yref = "paper",
        x = 1, y = -.175,
        text = "Années de suivi",
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
    PlotChroniques
  }
}

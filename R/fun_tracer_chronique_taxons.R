#' Title
#'
#' @param liste_station
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr mutate group_by summarise group_split
#' @importFrom ggplot2 ggplot geom_point aes scale_x_continuous sec_axis theme_minimal theme element_blank element_text labs
#' @importFrom lubridate year
#' @importFrom purrr map
tracer_chroniques_taxons <- function(liste_station) {
  liste_station %>%
    dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
    dplyr::group_by(code_station_hydrobio, libelle_station_hydrobio,
                    annee, libelle_support, libelle_taxon) %>%
    dplyr::summarise(resultat_taxon = sum(resultat_taxon), .group = "drop") %>%
    dplyr::group_by(libelle_support) %>%
    dplyr::group_split() %>%
    purrr::map(
      function(df_temp) {
        df_temp %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              x = annee,
              y = libelle_taxon,
              size = sqrt(resultat_taxon)
            )
          ) +
          ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(~.)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(hjust = .95),
            legend.position = "none",
            axis.text.y = ggplot2::element_text(hjust = 0, size = 10)
          ) +
          ggplot2::labs(
            x = "", y = ""
          )
      }
    )

}
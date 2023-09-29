#' Title
#'
#' @param listes_station
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr group_by summarise n_distinct mutate arrange select
#' @importFrom lubridate year
#' @importFrom purrr map_chr
#' @importFrom stringr str_split
resumer_listes <- function(listes_stations) {
  listes_stations %>%
    dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
    dplyr::group_by(code_station_hydrobio, code_support, libelle_support, annee) %>%
    dplyr::summarise(nb_taxa = dplyr::n_distinct(code_appel_taxon)) %>%
    dplyr::group_by(code_station_hydrobio, code_support, libelle_support) %>%
    dplyr::summarise(
      periode = paste0(
        as.character(min(annee)), "-", as.character(max(annee))
        ),
      n_op = dplyr::n_distinct(annee),
      nb_taxa = paste0(
        as.character(min(nb_taxa)), "-", as.character(max(nb_taxa))
        ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      eqb = libelle_support %>%
        stringr::str_split(pattern = " ") %>%
        purrr::map_chr(.f = function(x) x[1]) %>%
        factor(levels = c("Diatomées", "Macrophytes", "Macroinvertébrés", "Poissons"))
    ) %>%
    dplyr::arrange(eqb) %>%
    dplyr::select(
      code_station_hydrobio,
      code_support,
      EQB = eqb,
      `Période` = periode,
      `Années` = n_op,
      `Taxons` = nb_taxa
    )
}

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
resumer_listes <- function(listes_station) {
  listes_station %>%
    dplyr::group_by(libelle_support) %>%
    dplyr::summarise(
      periode = paste0(
        as.character(lubridate::year(min(date_prelevement))),
        "-",
        as.character(lubridate::year(max(date_prelevement)))),
      n_op = dplyr::n_distinct(lubridate::year(date_prelevement)),
      nb_taxa = dplyr::n_distinct(code_appel_taxon)
    ) %>%
    dplyr::mutate(
      eqb = libelle_support %>%
        stringr::str_split(pattern = " ") %>%
        purrr::map_chr(.f = function(x) x[1]) %>%
        factor(levels = c("Diatomées", "Macrophytes", "Macroinvertébrés", "Poissons"))
    ) %>%
    dplyr::arrange(eqb) %>%
    dplyr::select(
      EQB = eqb,
      `Période` = periode,
      `Nombre d'années` = n_op,
      `Nombre de taxons` = nb_taxa
    )
}

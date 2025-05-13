#' Résumer les listes faunistiques et floristiques d'une station
#'
#' @param listes_station Un data.frame contenant les listes faunistiques et floristiques
#'   avec les colonnes code_station_hydrobio, code_support, libelle_support, date_prelevement,
#'   libelle_taxon
#'
#' @return Un data.frame contenant un résumé des listes par élément de qualité biologique
#'   avec les colonnes suivantes :
#'   \itemize{
#'     \item code_station_hydrobio : Code de la station
#'     \item code_support : Code de l'élément de qualité biologique
#'     \item EQB : Libellé de l'élément de qualité biologique
#'     \item Période : Période de suivi (année min-année max)
#'     \item Années : Nombre d'années de suivi
#'     \item Taxons : Gamme du nombre de taxons (min-max)
#'   }
#' @export
#'
#' @details Cette fonction résume les listes faunistiques et floristiques en calculant
#'   le nombre d'années de suivi et la gamme du nombre de taxons observés par élément
#'   de qualité biologique. Les éléments de qualité sont ordonnés selon : Diatomées,
#'   Macrophytes, Macroinvertébrés, Poissons.
#'
#' @examples
#' \dontrun{
#' resume <- resumer_listes(listes_stations)
#' }
#' @importFrom dplyr group_by summarise n_distinct mutate arrange select
#' @importFrom lubridate year
#' @importFrom purrr map_chr
#' @importFrom stringr str_split
resumer_listes <- function(listes_stations) {
  listes_stations %>%
    dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
    dplyr::group_by(code_station_hydrobio, code_support, libelle_support, annee) %>%
    dplyr::summarise(nb_taxa = dplyr::n_distinct(libelle_taxon), .groups = "drop") %>%
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

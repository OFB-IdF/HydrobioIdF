#' Filtrer les informations d'une station
#'
#' @param stations Un objet sf contenant les stations hydrobiologiques
#' @param choix_station Le code de la station à filtrer (NULL pour toutes les stations)
#'
#' @return Un data.frame contenant les informations de base de la/des station(s) sélectionnée(s)
#' @export
#'
#' @examples
#' \dontrun{
#' stations_filtrees <- filtrer_station(stations, "03079780")
#' }
#' @importFrom dplyr filter select
#' @importFrom sf st_drop_geometry
filtrer_station <- function(stations, choix_station) {
  if (!is.null(choix_station)) {
    stations %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(
        code_station_hydrobio == choix_station
        ) %>%
      dplyr::select(
        code_station_hydrobio, libelle_station_hydrobio,
        uri_station_hydrobio
      )
  } else {
    stations %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        code_station_hydrobio, libelle_station_hydrobio,
        uri_station_hydrobio
      )
    }

}

#' Filtrer les indices biologiques d'une station
#'
#' @param stations Un objet sf contenant les stations hydrobiologiques
#' @param indices Un data.frame contenant les indices biologiques
#' @param choix_station Le code de la station à filtrer (NULL pour toutes les stations)
#' @param choix_eqb Les codes des éléments de qualité biologique à conserver
#'
#' @return Un data.frame contenant les indices biologiques filtrés pour la/les station(s) et élément(s) de qualité sélectionné(s)
#' @export
#'
#' @examples
#' \dontrun{
#' indices_filtres <- filtrer_indices(stations, indices, "03079780", c("POISSON", "DIATOMEES"))
#' }
#' @importFrom dplyr left_join filter
filtrer_indices <- function(stations, indices, choix_station, choix_eqb) {
  if (!is.null(choix_eqb)) {
    filtrer_station(stations, choix_station) %>%
      dplyr::left_join(
        indices %>%
          dplyr::filter(code_support %in% choix_eqb),
        by = c("code_station_hydrobio"),
        multiple = "all"
      )
  } else {
    filtrer_station(stations, choix_station) %>%
      dplyr::left_join(
        indices,
        by = c("code_station_hydrobio"),
        multiple = "all"
      )
  }

}

#' Filtrer les listes taxonomiques d'une station
#'
#' @param stations Un objet sf contenant les stations hydrobiologiques
#' @param listes_taxo Un data.frame contenant les listes taxonomiques
#' @param choix_station Le code de la station à filtrer (NULL pour toutes les stations)
#' @param choix_eqb Les codes des éléments de qualité biologique à conserver
#'
#' @return Un data.frame contenant les listes taxonomiques filtrées pour la/les station(s) et élément(s) de qualité sélectionné(s)
#' @export
#'
#' @examples
#' \dontrun{
#' listes_filtrees <- filtrer_listes(stations, listes_taxo, "03079780", c("POISSON", "DIATOMEES"))
#' }
#' @importFrom dplyr left_join select filter
filtrer_listes <- function(stations, listes_taxo, choix_station, choix_eqb) {
  if (!is.null(choix_eqb)) {
    filtrer_station(stations, choix_station) %>%
      dplyr::left_join(
        listes_taxo %>%
          dplyr::select(-libelle_station_hydrobio) %>%
          dplyr::filter(code_support %in% choix_eqb),
        by = "code_station_hydrobio",
        multiple = "all"
      )
  } else {
    filtrer_station(stations, choix_station) %>%
      dplyr::left_join(
        listes_taxo %>%
          dplyr::select(-libelle_station_hydrobio),
        by = "code_station_hydrobio",
        multiple = "all"
      )
  }

}

#' Filtrer les résumés des listes taxonomiques
#'
#' @param resumes_listes Un data.frame contenant les résumés des listes taxonomiques
#' @param choix_station Le code de la station à filtrer (NULL pour toutes les stations)
#' @param choix_eqb Les codes des éléments de qualité biologique à conserver (NULL pour tous les EQB)
#'
#' @return Un data.frame contenant les résumés des listes taxonomiques filtrés pour la/les station(s) et élément(s) de qualité sélectionné(s)
#' @export
#'
#' @examples
#' \dontrun{
#' resumes_filtres <- filtrer_resumes(resumes_listes, "03079780", c("POISSON", "DIATOMEES"))
#' }
filtrer_resumes <- function(resumes_listes, choix_station, choix_eqb) {
  if(is.null(choix_station)) choix_station <- unique(resumes_listes$code_station_hydrobio)
  if (is.null(choix_eqb)) choix_eqb <- unique(resumes_listes$code_support)

  resumes_listes %>%
    dplyr::filter(
      code_station_hydrobio %in% choix_station,
      code_support %in% choix_eqb
      )
}

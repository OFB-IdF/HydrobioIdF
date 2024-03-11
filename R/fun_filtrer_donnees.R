#' Title
#'
#' @param choix_station
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param choix_station
#' @param choix_eqb
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param choix_station
#' @param choix_eqb
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param choix_station
#' @param choix_eqb
#'
#' @return
#' @export
#'
#' @examples
filtrer_resumes <- function(resumes_listes, choix_station, choix_eqb) {
  if(is.null(choix_station)) choix_station <- unique(resumes_listes$code_station_hydrobio)
  if (is.null(choix_eqb)) choix_eqb <- unique(resumes_listes$code_support)

  resumes_listes %>%
    dplyr::filter(
      code_station_hydrobio %in% choix_station,
      code_support %in% choix_eqb
      )
}

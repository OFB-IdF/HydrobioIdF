#' @param code_departement
#' @param suivi_regie
#'
#' @export
#'
#' @importFrom dplyr select mutate
#' @importFrom hubeau get_hydrobio_stations_hydrobio
#' @importFrom sf st_as_sf
telecharger_stations <- function(code_departement, suivi_regie) {
  hubeau::get_hydrobio_stations_hydrobio(
    code_departement = paste(code_departement, collapse = ",")
    ) %>%
    dplyr::select(
      code_station_hydrobio, libelle_station_hydrobio,
      uri_station_hydrobio, coordonnee_x, coordonnee_y,
      code_cours_eau, libelle_cours_eau, code_masse_eau,
      libelle_masse_eau, code_departement,
      date_premier_prelevement, date_dernier_prelevement
      ) %>%
    sf::st_as_sf(
      coords = c("coordonnee_x", "coordonnee_y"),
      crs = 2154,
      remove=FALSE
      ) %>%
    dplyr::mutate(
      regie = code_station_hydrobio %in% suivi_regie$code_station
    )
}

#' @param code_departement
#' @param code_indice
#'
#' @export
#'
#' @importFrom dplyr distinct mutate filter
#' @importFrom hubeau get_hydrobio_indices
#' @importFrom lubridate as_date year
#' @importFrom sf st_as_sf
telecharger_indices <- function(code_departement, code_indice = c(`IBG-eq` = 5910, `I2M2` = 7613, `MIV-GCE` = 6951, `IBMR` = 2928, `IBD` = 5856, `IPR` = 7036)) {
  hubeau::get_hydrobio_indices(
    list(
      code_departement = paste(code_departement, collapse = ","),
      code_indice = paste(code_indice, collapse = ",")
      )
    ) %>%
    sf::st_as_sf(
      coords = c("coordonnee_x", "coordonnee_y"),
      crs = 2154,
      remove = FALSE
      ) %>%
  dplyr::distinct(
    code_station_hydrobio, code_support, libelle_support,
    code_prelevement, date_prelevement, code_indice, libelle_indice,
    resultat_indice, code_qualification, libelle_qualification
    ) %>%
    dplyr::mutate(
      date_prelevement = lubridate::as_date(date_prelevement)
      ) %>%
    dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
    dplyr::filter(!is.na(resultat_indice))
}

#' Title
#'
#' @param code_departement
#' @param code_eqb
#'
#' @return
#' @export
#'
#' @importFrom dplyr group_by summarise pull
#' @importFrom hubeau get_hydrobio_taxons get_hydrobio_stations_hydrobio
#' @importFrom purrr map list_rbind
telecharger_listes <- function(code_departement, code_eqb = c(`Poissons` = 4, `Diatomées` = 10, `Macroinvertébrés` = 13, `Macrophytes` = 27)) {

  listes <- try(
    hubeau::get_hydrobio_taxons(
      code_departement = paste(code_departement, collapse = ","),
      code_support = paste(code_eqb, collapse = ",")
    ) %>%
      dplyr::group_by(
        code_station_hydrobio, libelle_station_hydrobio,
        code_prelevement, date_prelevement,
        code_support, libelle_support, code_appel_taxon, libelle_appel_taxon,
        coordonnee_x, coordonnee_y
      ) %>%
      dplyr::summarise(
        resultat_taxon = sum(resultat_taxon),
        .groups = "drop"
      )
  )

  if (class(listes) == "try-error") {
    stations <- hubeau::get_hydrobio_stations_hydrobio(
      code_departement = paste(code_departement, collapse = ",")
    ) %>%
      dplyr::pull(code_station_hydrobio)

    listes <- purrr::map(
      .x = stations,
      .f = function (x) {
        hubeau::get_hydrobio_taxons(
          code_station_hydrobio = x,
          code_support = paste(code_eqb, collapse = ",")
        ) %>%
          (function(df_temp) {
            if (nrow(df_temp) != 0) {
              df_temp %>%
                dplyr::group_by(
                  code_station_hydrobio, libelle_station_hydrobio,
                  code_prelevement, date_prelevement,
                  code_support, libelle_support, code_appel_taxon, libelle_appel_taxon,
                  coordonnee_x, coordonnee_y
                ) %>%
                dplyr::summarise(
                  resultat_taxon = sum(resultat_taxon),
                  .groups = "drop"
                )
            } else {
              df_temp
            }
          })
      },
      .progress = TRUE
    ) %>%
      purrr::list_rbind()
  }

  listes
}


#' Title
#'
#' @param chemin_xlsx
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate filter select case_when bind_rows
#' @importFrom janitor clean_names
#' @importFrom openxlsx2 read_xlsx
#' @importFrom purrr map_dfr
#' @importFrom stringr str_extract_all
#' @importFrom tidyr pivot_longer
importer_suivis_regie <- function(chemin_xlsx) {
  openxlsx2::read_xlsx(chemin_xlsx) %>%
    janitor::clean_names() %>%
    (function(df_xl) {
      colnames(df_xl)[seq(3)] <- df_xl[1,seq(3)] %>%
        janitor::make_clean_names()

      indices <- df_xl[1,-seq(4)] %>%
        t() %>%
        as.vector() %>%
        unique() %>%
        na.omit()

      purrr::map_dfr(
        indices,
        function(i) {
          df_xl[-1,c(seq(4), which(as.vector(t(df_xl[1,])) == i))] %>%
            tidyr::pivot_longer(
              cols = -seq(4),
              names_to = "annee",
              values_to = "realisation"
            ) %>%
            dplyr::mutate(
              annee = annee %>%
                stringr::str_extract_all(
                  pattern = "\\d{4}"
                ) %>%
                as.numeric(),
              indice = i,
              code_station = paste0("0", code_station)
            ) %>%
            dplyr::filter(realisation %in%  c("0", "1")) %>%
            dplyr::select(
              cours_deau, commune, code_station, indice, annee, realisation
            )
        }
      ) %>%
        dplyr::mutate(
          code_indice = dplyr::case_when(
            indice == "IBD" ~ "5856",
            indice == "MPCE" ~ "5910",
            indice == "IBMR" ~ "2928"
          )
        ) %>%
        (function(df) {
          dplyr::bind_rows(
            df,
            df %>%
              dplyr::filter(indice == "MPCE") %>%
              dplyr::mutate(code_indice = "7613")
          )
        })

    })
}

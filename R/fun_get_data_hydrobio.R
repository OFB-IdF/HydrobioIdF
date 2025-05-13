#' Télécharger les stations hydrobiologiques
#'
#' @param code_departement Un vecteur de codes départements (format : "01", "02", etc.)
#' @param suivi_regie Un data.frame contenant les stations suivies en régie
#'
#' @return Un objet sf contenant les stations hydrobiologiques avec leurs coordonnées et attributs
#' @export
#'
#' @details Cette fonction télécharge les stations hydrobiologiques depuis l'API Hub'Eau et
#' ajoute une colonne indiquant si la station est suivie en régie.
#'
#' @examples
#' \dontrun{
#' stations <- telecharger_stations(c("75", "77", "78"), suivi_regie)
#' }
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

#' Télécharger les indices biologiques
#'
#' @param code_departement Un vecteur de codes départements (format : "01", "02", etc.)
#' @param code_indice Un vecteur nommé de codes indices. Par défaut : IBG-eq (5910), I2M2 (7613),
#'   MIV-GCE (6951), IBMR (2928), IBD (5856), IPR (7036)
#'
#' @return Un objet sf contenant les indices biologiques avec leurs coordonnées et attributs
#' @export
#'
#' @details Cette fonction télécharge les indices biologiques depuis l'API Hub'Eau pour les
#' départements et indices spécifiés. Les indices sont géolocalisés et incluent les dates
#' de prélèvement.
#'
#' @examples
#' \dontrun{
#' indices <- telecharger_indices(c("75", "77", "78"))
#' }
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

#' Télécharger les listes faunistiques et floristiques
#'
#' @param code_departement Un vecteur de codes départements (format : "01", "02", etc.)
#' @param code_eqb Un vecteur nommé de codes des éléments de qualité biologique. Par défaut :
#'   Poissons (4), Diatomées (10), Macroinvertébrés (13), Macrophytes (27)
#'
#' @return Un data.frame contenant les listes faunistiques et floristiques avec leurs coordonnées
#'   et attributs, incluant les abondances des taxons par prélèvement
#' @export
#'
#' @details Cette fonction télécharge les listes faunistiques et floristiques depuis l'API Hub'Eau
#'   pour les départements et éléments de qualité biologique spécifiés. En cas d'erreur lors du
#'   téléchargement global, la fonction tente de télécharger les données station par station.
#'   Les abondances des taxons sont sommées par prélèvement.
#'
#' @examples
#' \dontrun{
#' listes <- telecharger_listes(c("75", "77", "78"))
#' }
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
        taxons <- try(
          hubeau::get_hydrobio_taxons(
            code_station_hydrobio = x,
            code_support = paste(code_eqb, collapse = ",")
            )
          )

        trials <- 1
        while (all(class(taxons) == "try-error" & trials <= 10)) {
          taxons <- try(
            hubeau::get_hydrobio_taxons(
              code_station_hydrobio = x,
              code_support = paste(code_eqb, collapse = ",")
            )
          )

          trials <- trials + 1
        }

        if (any(class(taxons) == "try-error")) {
          warning(paste0("Station ", x, ": erreur de téléchargement des listes"))
        } else {
          taxons %>%
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
        }
      },
      .progress = TRUE
    ) %>%
      purrr::list_rbind()
  }

  listes
}


#' Importer les données de suivi en régie
#'
#' @param chemin_xlsx Le chemin vers le fichier Excel contenant les données de suivi en régie
#'
#' @return Un data.frame contenant les informations de suivi en régie avec les colonnes suivantes :
#'   cours_deau, commune, code_station, indice, annee, realisation, code_indice
#' @export
#'
#' @details Cette fonction importe les données de suivi en régie à partir d'un fichier Excel.
#'   Elle nettoie les noms de colonnes, pivote les données pour obtenir un format long,
#'   et ajoute les codes indices correspondant aux différents protocoles (IBD, MPCE, IBMR).
#'   Pour les MPCE, elle duplique les lignes pour ajouter le code I2M2 en plus du code IBG.
#'
#' @examples
#' \dontrun{
#' suivi_regie <- importer_suivis_regie("chemin/vers/fichier.xlsx")
#' }
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
              indice = i
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

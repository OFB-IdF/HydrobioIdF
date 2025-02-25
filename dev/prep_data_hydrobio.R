if (!require(pak)) install.packages("pak")
pak::pkg_install(c("OFB-IdF/HydrobioIdF", "CedricMondy/SEEEapi"))

unlink("dev/data_hydrobio.rda")

date_donnees <- Sys.Date()

regie <- HydrobioIdF::importer_suivis_regie("dev/Historique prog labo.xlsx")
typo_nationale <- sf::st_read("dev/stations_reseaux_sn.gpkg", layer = "stations_reseaux_sn") |>
  dplyr::select(CdStationMesureEauxSurface, TypeCEStationMesureEauxSurface) |>
  sf::st_drop_geometry()

departements <- c(75, 77, 78, 91, 92, 93, 94, 95)
departements_extra <- c(10, 51, 52, 45)

stations <- HydrobioIdF::telecharger_stations(
  code_departement = c(departements, departements_extra),
  suivi_regie = regie
  ) |>
  dplyr::filter(
    (code_departement %in% departements) |  regie
  ) |>
  dplyr::left_join(
    typo_nationale, by = c("code_station_hydrobio" = "CdStationMesureEauxSurface")
  )

indices <- HydrobioIdF::telecharger_indices(
  code_departement = c(departements, departements_extra)
) |>
  dplyr::filter(
    code_station_hydrobio %in% stations$code_station_hydrobio
  )

indices_seee <- indices |>
  dplyr::transmute(
    CODE_OPERATION = code_prelevement,
    CODE_STATION = code_station_hydrobio,
    DATE = date_prelevement |>
      format(format = "%d/%m/%Y"),
    CODE_PAR = code_indice,
    LIB_PAR = libelle_indice,
    RESULTAT = resultat_indice
  ) |>
  dplyr::bind_rows(
    indices |>
      dplyr::filter(code_indice == 7036) |>
      dplyr::transmute(
        CODE_OPERATION = code_prelevement,
        CODE_STATION = code_station_hydrobio,
        DATE = date_prelevement |>
          format(format = "%d/%m/%Y")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        CODE_PAR = "NA",
        LIB_PAR = "ALT",
        RESULTAT = 200 # valeur bidon
      )
  ) |>
  dplyr::mutate(
    LIB_PAR = ifelse(CODE_PAR == 7036, "IPR", LIB_PAR)
  )

stations_seee <- stations |>
  dplyr::transmute(
    CODE_STATION = code_station_hydrobio,
    TYPO_NATIONALE = TypeCEStationMesureEauxSurface,
    TG_BV = TypeCEStationMesureEauxSurface |>
      stringr::str_detect(pattern = "TG") |>
      stringr::str_replace_all(pattern = 'TRUE', replacement = "OUI") |>
      stringr::str_replace_all(pattern = 'FALSE', replacement = "NON"),
    PERIODE_DEBUT = lubridate::year(date_premier_prelevement),
    PERIODE_FIN = lubridate::year(date_dernier_prelevement)
  ) |>
  dplyr::filter(!is.na(TYPO_NATIONALE)) |>
  dplyr::rowwise() |>
  dplyr::mutate(ANNEE = list(seq(PERIODE_DEBUT, PERIODE_FIN))) |>
  dplyr::ungroup() |>
  tidyr::unnest(ANNEE) |>
  dplyr::select(CODE_STATION, TYPO_NATIONALE, TG_BV, PERIODE_DEBUT = ANNEE, PERIODE_FIN = ANNEE)

# SEEEapi::get_algo(
#   indic = "EBio_CE_2018",
#   version = "1.0.1",
#   dir_algo = "algo_seee"
# )

# SEEEapi::get_algo(
#   indic = "EBio_CE_2015",
#   version = "1.0.1",
#   dir_algo = "algo_seee"
# )

etat_bio <- (SEEEapi::calc_indic(
    indic = "EBio_CE_2018",
    version = "1.0.1",
    locally = TRUE,
    dir_algo = "algo_seee",
    data = list(
      stations_seee,
      indices_seee |>
        dplyr::filter(CODE_PAR == 5856),
      indices_seee |>
        dplyr::filter(CODE_PAR == 2928),
      indices_seee |>
        dplyr::filter(CODE_PAR == 7613),
      indices_seee |>
        dplyr::filter(CODE_PAR  %in% c("NA", "7036") ) |>
        dplyr::arrange(CODE_STATION, CODE_OPERATION, CODE_PAR)
    )
  )$result) |>
  dplyr::bind_rows(
    SEEEapi::calc_indic(
      indic = "EBio_CE_2018",
      version = "1.0.1",
      locally = TRUE,
      dir_algo = "algo_seee",
      data = list(
        stations_seee,
        indices_seee |>
          dplyr::slice(0),
        indices_seee |>
          dplyr::slice(0),
        indices_seee |>
          dplyr::filter(CODE_PAR == 6951),
        indices_seee |>
          dplyr::slice(0)
      )
    )$result
  ) |>
  dplyr::bind_rows(
    SEEEapi::calc_indic(
      indic = "EBio_CE_2015",
      version = "1.0.1",
      locally = TRUE,
      dir_algo = "algo_seee",
      data = list(
        stations_seee,
        indices_seee |>
          dplyr::slice(0),
        indices_seee |>
          dplyr::slice(0),
        indices_seee |>
          dplyr::filter(CODE_PAR == 5910),
        indices_seee |>
          dplyr::slice(0)
      )
    )$result
  ) |>
  dplyr::filter(!is.na(RESULTAT)) |>
  dplyr::select(
    code_station_hydrobio = CODE_STATION,
    annee = PERIODE_DEBUT,
    code_indice = CODE_PAR,
    libelle_indice = LIB_PAR,
    resultat_indice = RESULTAT,
    eqr_indice = EQR,
    classe_indice = CLASSE
  ) |>
  dplyr::mutate(
    dplyr::across(c(resultat_indice, eqr_indice, annee), as.numeric)
  ) |>
  dplyr::left_join(
    indices |>
      dplyr::distinct(code_indice, code_support, libelle_support),
    by = "code_indice"
  )

etat_bio <- etat_bio |>
  dplyr::bind_rows(
    indices |>
      dplyr::anti_join(etat_bio, by = c("code_indice", "code_station_hydrobio", "annee")) |>
      dplyr::select(
        code_station_hydrobio, annee, code_indice, libelle_indice, resultat_indice, code_support, libelle_support
      )
  ) |>
  dplyr::mutate(
    libelle_indice = dplyr::case_when(
      code_indice == 7036 ~ "IPR",
      code_indice == 5856 ~ "IBD",
      code_indice == 2928 ~ "IBMR",
      code_indice == 7613 ~ "I2M2",
      code_indice == 5910 ~ "IBG équivalent",
      code_indice == 6951 ~ "Invertébrés GCE",
      TRUE ~ libelle_indice
    )
  )

fichiers_parametres <- list.files(
  path = "algo_seee/EBio_CE_2018/1.0.1",
  pattern = "params",
  full.names = TRUE
)
fichiers_parametres <- c(
  fichiers_parametres[!stringr::str_detect(fichiers_parametres, "IBG-DCE")],
  "algo_seee/EBio_CE_2015/1.0.1/EBio_CE_2015_params_IBG-DCE.csv"
)

noms_indices_param <- fichiers_parametres |>
  stringr::str_remove(
    pattern = "algo_seee/EBio_CE_201\\d/1.0.1/EBio_CE_201\\d_params_"
) |>
  stringr::str_remove(
    pattern = ".csv"
  )

valeurs_seuils <- fichiers_parametres |>
  purrr::map(
    function(x) {
      vroom::vroom(file = x, locale = vroom::locale(decimal_mark = ","))
    }
  ) |>
  purrr::set_names(noms_indices_param)

parametres_eqr <- c("IBD", "IBMR", "IBG-DCE", "MGCE") |>
  purrr::map(
    function(i) {
      if (i == "IBD") {
        params <- valeurs_seuils[[i]] |>
          dplyr::distinct(TYPO_NATIONALE, TG_BV, REFERENCE, MINIMUM)
      }

      if (i %in% c("IBG-DCE", "MGCE", "IBMR")) {
        params <- valeurs_seuils[[i]] |>
          dplyr::distinct(TYPO_NATIONALE, REFERENCE)
      }

      params
    }
  ) |>
  purrr::set_names(c(5856, 2928, 5910, 6951))

for (x in noms_indices_param) {
  print(x)
  if (x == "IPR") {
    valeurs_seuils$IPR <- valeurs_seuils$IPR |>
      dplyr::mutate(
        bon = ifelse(is.na(BON), BASSE_ALTITUDE, BON)
      ) |>
      dplyr::select(TYPO_NATIONALE, TRES_BON, BON = bon, MOYEN, MEDIOCRE, MAUVAIS) |>
      tidyr::pivot_longer(
        cols = -TYPO_NATIONALE,
        names_to = "classe", values_to = "seuil_haut"
      ) |>
      dplyr::mutate(
        seuil_bas = ifelse(classe == "TRES_BON", 0, dplyr::lag(seuil_haut))
      )
  } else {
    if (x == "IBD") {
      valeurs_seuils$IBD <- valeurs_seuils$IBD |>
        dplyr::select(TYPO_NATIONALE, TG_BV, TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS) |>
        tidyr::pivot_longer(
          cols = -c(TYPO_NATIONALE, TG_BV),
          names_to = "classe", values_to = "seuil_bas"
        ) |>
        dplyr::mutate(
          seuil_haut = ifelse(classe == "TRES_BON", 1, dplyr::lag(seuil_bas))
        )
    } else {
      valeurs_seuils[[x]] <- valeurs_seuils[[x]] |>
        dplyr::select(TYPO_NATIONALE, TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS) |>
        tidyr::pivot_longer(
          cols = -c(TYPO_NATIONALE),
          names_to = "classe", values_to = "seuil_bas"
        ) |>
        dplyr::mutate(
          seuil_haut = ifelse(classe == "TRES_BON", 1, dplyr::lag(seuil_bas))
        )
    }
  }
}

valeurs_seuils_stations <- stations_seee |>
  sf::st_drop_geometry() |>
  dplyr::distinct(CODE_STATION, TYPO_NATIONALE, TG_BV) |>
  dplyr::mutate(indice = "IBD") |>
  dplyr::left_join(
    valeurs_seuils$IBD,
    by = c("TYPO_NATIONALE", "TG_BV")
  )

for (i in noms_indices_param) {
  if (i != "IBD") {
    valeurs_seuils_stations <- dplyr::bind_rows(
      valeurs_seuils_stations,
      stations_seee |>
        sf::st_drop_geometry() |>
        dplyr::distinct(CODE_STATION, TYPO_NATIONALE, TG_BV) |>
        dplyr::mutate(indice = i) |>
        dplyr::left_join(
          valeurs_seuils[[i]],
          by = c("TYPO_NATIONALE")
        )
    )
  }
}

valeurs_seuils_stations <- valeurs_seuils_stations |>
  dplyr::mutate(
    seuil_haut = ifelse(is.na(seuil_bas), NA, seuil_haut)
  )

listes_taxo <- HydrobioIdF::telecharger_listes(
  code_departement = c(departements, departements_extra)
) |>
  dplyr::filter(
    code_station_hydrobio %in% stations$code_station_hydrobio
  ) |>
  dplyr::mutate(
    libelle_taxon = libelle_appel_taxon |>
      stringr::str_remove_all(pattern = " var\\. .*$") |>
      stringr::str_remove_all(pattern = " abnormal form.*$") |>
      stringr::str_remove_all(pattern = " f\\. .*$") |>
      stringr::str_remove_all(pattern = " ssp\\. .*$")
  )

acronymes_indices <- c(
  `5856` = "IBD",
  `2928` = "IBMR",
  `7613` = "I2M2",
  `5910` = "IBG équivalent",
  `6951` = "Invertébrés GCE",
  `7036` = "IPR"
)

donnees_carte <- stations |>
  dplyr::left_join(
    indices |>
      dplyr::mutate(
        annee = lubridate::year(date_prelevement)
      ) |>
      dplyr::mutate(
        code_indice = code_indice |>
          factor(levels = names(acronymes_indices))
        ) |>
      dplyr::group_by(code_station_hydrobio, code_support, libelle_support) |>
      dplyr::reframe(
        derniere_annee = max(annee),
        dernier_indice = code_indice[annee == max(annee)],
        dernier_resultat = resultat_indice[annee == max(annee)],
        nb_annees = dplyr::n_distinct(annee)
        ) |>
      dplyr::arrange(code_station_hydrobio, libelle_support, dernier_indice) |>
      dplyr::mutate(
        dernier_resultat = paste0(acronymes_indices[as.character(dernier_indice)],  ": ", dernier_resultat)
      ) |>
      dplyr::select(-dernier_indice) |>
      dplyr::group_by(code_station_hydrobio, code_support, libelle_support, derniere_annee, nb_annees) |>
      dplyr::summarise(
        derniers_resultats = paste(dernier_resultat, collapse = "/"),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        derniers_resultats = paste0(derniers_resultats, " (", derniere_annee, ")")
        ),
    by = "code_station_hydrobio",
    multiple ="all"
  ) |>
  dplyr::filter(!is.na(libelle_support)) |>
  sf::st_transform(crs = 4326)

donnees_carte_taxons <-
  dplyr::left_join(
    stations |>
      dplyr::select(code_departement, code_station_hydrobio),
    listes_taxo |>
  dplyr::group_by(code_station_hydrobio, libelle_station_hydrobio, code_support, libelle_taxon) |>
  dplyr::summarise(
    derniers_resultats = max(date_prelevement),
    ab_moy = mean(resultat_taxon),
    resume = paste0(
      "abondance: ",
      unique(range(resultat_taxon)) |>
        paste(collapse = "-"),
      " (",
      unique(range(lubridate::year(date_prelevement))) |>
        paste(collapse = "-"),
      ")"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    hover = paste0(
      "<b>", libelle_taxon, "</b><br>",
      "<em>", libelle_station_hydrobio, "</em><br><br>",
      resume
    )
  ),
    by = "code_station_hydrobio", multiple = "all"
  ) |>
  sf::st_transform(crs = 4326)

resumes_listes <- HydrobioIdF::resumer_listes(listes_taxo)


save(date_donnees, regie, stations, indices, valeurs_seuils_stations, etat_bio, listes_taxo, resumes_listes, acronymes_indices, donnees_carte, donnees_carte_taxons, etat_bio, parametres_eqr,
     file = "dev/data_hydrobio.rda")



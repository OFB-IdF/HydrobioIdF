if (!require(pak)) install.packages("pak")
pak::pkg_install(c("OFB-IdF/HydrobioIdF", "CedricMondy/SEEEapi"))

date_donnees <- Sys.Date()

regie <- HydrobioIdF::importer_suivis_regie("dev/Historique prog labo.xlsx")
typo_nationale <- sf::st_read("dev/stations_reseaux_sn.gpkg", layer = "stations_reseaux_sn") |> 
  dplyr::select(CdStationMesureEauxSurface, TypeCEStationMesureEauxSurface) |> 
  sf::st_drop_geometry()

departements <- c(75, 77, 78, 91, 92, 93, 94, 95)
departements_extra <- c(10, 51, 52)

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


# etat_bio <- SEEEapi::calc_indic(
#     indic = "EBio_CE_2018",
#     version = "1.0.1",
#     data = list(
#       stations_seee #|> readr::write_delim(file = "stations.txt", delim = "\t")
#       ,
#       indices_seee |> 
#         dplyr::filter(CODE_PAR == 5856) #|> readr::write_delim(file = "ibd.csv", delim = ";")
#       ,
#       indices_seee |> 
#         dplyr::filter(CODE_PAR == 2928) #|> readr::write_delim(file = "ibmr.csv", delim = ";")
#       ,
#       indices_seee |> 
#         dplyr::filter(CODE_PAR == 7613) #|> readr::write_delim(file = "i2m2.csv", delim = ";")
#       ,
#       indices_seee |> 
#         dplyr::filter(CODE_PAR  %in% c("NA", "7036") ) |> 
#         dplyr::arrange(CODE_STATION, CODE_OPERATION, CODE_PAR) #|> readr::write_delim(file = "ipr.csv", delim = ";")
#     )
#   )

etat_bio <- vroom::vroom("RESULTAT_EBio_CE_2018_1.0.1_2024-12-11-16-16-14.csv", skip = 1) |> 
  dplyr::filter(!is.na(RESULTAT))

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


save(date_donnees, regie, stations, indices, listes_taxo, resumes_listes, acronymes_indices, donnees_carte, donnees_carte_taxons, etat_bio,
     file = "dev/data_hydrobio.rda")



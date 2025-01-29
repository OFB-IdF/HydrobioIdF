if (!require(pak)) install.packages("pak")
pak::pkg_install("OFB-IdF/HydrobioIdF")

date_donnees <- Sys.Date()

regie <- HydrobioIdF::importer_suivis_regie("dev/Historique prog labo.xlsx")

departements <- c(75, 77, 78, 91, 92, 93, 94, 95)
departements_extra <- c(10, 51, 52, 45)

stations <- HydrobioIdF::telecharger_stations(
  code_departement = c(departements, departements_extra),
  suivi_regie = regie
  ) |>
  dplyr::filter(
    (code_departement %in% departements) |  regie
  )

indices <- HydrobioIdF::telecharger_indices(
  code_departement = c(departements, departements_extra)
) |>
  dplyr::filter(
    code_station_hydrobio %in% stations$code_station_hydrobio
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


save(date_donnees, regie, stations, indices, listes_taxo, resumes_listes, acronymes_indices, donnees_carte, donnees_carte_taxons,
     file = "dev/data_hydrobio.rda")



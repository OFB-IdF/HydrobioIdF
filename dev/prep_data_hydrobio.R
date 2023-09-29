library(HydrobioIdF)

stations <- telecharger_stations(
  code_departement = c(75, 77, 78, 91, 92, 93, 94, 95, 10, 51),
  suivi_regie = HydrobioIdF:::regie
  ) %>%
  dplyr::filter(
    (code_departement %in% c(75, 77, 78, 91, 92, 93, 94, 95)) |  regie
  )

indices <- telecharger_indices(
  code_departement = c(75, 77, 78, 91, 92, 93, 94, 95, 10, 51)
) %>%
  dplyr::filter(
    code_station_hydrobio %in% stations$code_station_hydrobio
  )

listes_taxo <- telecharger_listes(
  code_departement = c(75, 77, 78, 91, 92, 93, 94, 95, 10, 51)
) %>%
  dplyr::filter(
    code_station_hydrobio %in% stations$code_station_hydrobio
  )

acronymes_indices <- c(
  `5856` = "IBD",
  `2928` = "IBMR",
  `7613` = "I2M2",
  `5910` = "IBG équivalent",
  `6951` = "Invertébrés GCE",
  `7036` = "IPR"
)

donnees_carte <- stations %>%
  dplyr::left_join(
    indices %>%
      dplyr::mutate(
        annee = lubridate::year(date_prelevement)
      ) %>%
      dplyr::mutate(
        code_indice = code_indice %>%
          factor(levels = names(acronymes_indices))
        ) %>%
      dplyr::group_by(code_station_hydrobio, code_support, libelle_support) %>%
      dplyr::reframe(
        derniere_annee = max(annee),
        dernier_indice = code_indice[annee == max(annee)],
        dernier_resultat = resultat_indice[annee == max(annee)],
        nb_annees = dplyr::n_distinct(annee)
        ) %>%
      dplyr::arrange(code_station_hydrobio, libelle_support, dernier_indice) %>%
      dplyr::mutate(
        dernier_resultat = paste0(acronymes_indices[as.character(dernier_indice)],  ": ", dernier_resultat)
      ) %>%
      dplyr::select(-dernier_indice) %>%
      dplyr::group_by(code_station_hydrobio, code_support, libelle_support, derniere_annee, nb_annees) %>%
      dplyr::summarise(
        derniers_resultats = paste(dernier_resultat, collapse = "/"),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        derniers_resultats = paste0(derniers_resultats, " (", derniere_annee, ")")
        ),
    by = "code_station_hydrobio",
    multiple ="all"
  ) %>%
  dplyr::filter(!is.na(libelle_support)) %>%
  sf::st_transform(crs = 4326)

resumes_listes <- resumer_listes(listes_taxo)

usethis::use_data(stations, indices, listes_taxo, resumes_listes, acronymes_indices, donnees_carte, overwrite = TRUE)



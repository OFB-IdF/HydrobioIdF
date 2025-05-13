#' Script de préparation des données hydrobiologiques
#'
#' Ce script prépare les données hydrobiologiques pour l'application HydrobioIdF en :
#' 1. Téléchargeant les données des stations et des indices biologiques
#' 2. Calculant l'état biologique selon les méthodes 2015 et 2018
#' 3. Préparant les données pour l'affichage cartographique
#' 4. Résumant les listes faunistiques et floristiques

# Installation des dépendances
if (!require(pak)) install.packages("pak")
pak::pkg_install(c("OFB-IdF/HydrobioIdF", "CedricMondy/SEEEapi"))

# Suppression du fichier de données s'il existe
unlink("dev/data_hydrobio.rda")

# Date de mise à jour des données
date_donnees <- Sys.Date()

# Import des suivis en régie et de la typologie nationale
regie <- HydrobioIdF::importer_suivis_regie("dev/Historique prog labo.xlsx")
typo_nationale <- sf::st_read("dev/stations_reseaux_sn.gpkg", layer = "stations_reseaux_sn") |>
  dplyr::select(CdStationMesureEauxSurface, TypeCEStationMesureEauxSurface) |>
  sf::st_drop_geometry()

# Définition des départements à traiter
departements <- c(75, 77, 78, 91, 92, 93, 94, 95)  # Départements d'Île-de-France
departements_extra <- c(10, 51, 52, 45)  # Départements limitrophes avec stations en régie

# Téléchargement et filtrage des stations
# - Récupération des stations des départements d'IdF et limitrophes
# - Conservation uniquement des stations d'IdF et des stations en régie
# - Association avec la typologie nationale pour le calcul des états
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

# Téléchargement des indices biologiques pour les stations sélectionnées
# Les indices concernés sont :
# - IPR (7036) : Indice Poissons Rivière
# - IBD (5856) : Indice Biologique Diatomées
# - IBMR (2928) : Indice Biologique Macrophytique en Rivière
# - I2M2 (7613) : Indice Invertébrés Multi-Métrique
# - IBG équivalent (5910) : Indice Biologique Global
# - Invertébrés GCE (6951) : Indice Groupe de Consommation Écologique
indices <- HydrobioIdF::telecharger_indices(
  code_departement = c(departements, departements_extra)
) |>
  dplyr::filter(
    code_station_hydrobio %in% stations$code_station_hydrobio
  )

# Formatage des données pour le SEEE
# Le SEEE requiert un format spécifique pour les données d'entrée :
# - CODE_OPERATION : identifiant unique du prélèvement
# - CODE_STATION : code de la station
# - DATE : date du prélèvement au format JJ/MM/AAAA
# - CODE_PAR : code de l'indice
# - LIB_PAR : libellé de l'indice
# - RESULTAT : valeur de l'indice
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
  # Ajout d'une ligne fictive pour l'altitude des stations IPR
  # Nécessaire pour le calcul de l'état biologique
  # La valeur de 200m est utilisée par défaut car elle n'influence pas
  # le calcul pour les cours d'eau d'Île-de-France
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
        RESULTAT = 200
      )
  ) |>
  dplyr::mutate(
    LIB_PAR = ifelse(CODE_PAR == 7036, "IPR", LIB_PAR)
  )

# Formatage des stations pour le SEEE
# Le SEEE requiert les informations suivantes pour chaque station :
# - CODE_STATION : code de la station
# - TYPO_NATIONALE : type de cours d'eau
# - TG_BV : présence de très grands bassins versants (>10 000 km²)
# - PERIODE_DEBUT et PERIODE_FIN : années de début et fin pour le calcul
stations_seee <- stations |>
  dplyr::transmute(
    CODE_STATION = code_station_hydrobio,
    TYPO_NATIONALE = TypeCEStationMesureEauxSurface,
    # Détection des très grands cours d'eau (préfixe TG dans la typologie)
    # Cette information est nécessaire pour le calcul de l'IBD
    TG_BV = TypeCEStationMesureEauxSurface |>
      stringr::str_detect(pattern = "TG") |>
      stringr::str_replace_all(pattern = 'TRUE', replacement = "OUI") |>
      stringr::str_replace_all(pattern = 'FALSE', replacement = "NON"),
    PERIODE_DEBUT = lubridate::year(date_premier_prelevement),
    PERIODE_FIN = lubridate::year(date_dernier_prelevement)
  ) |>
  # Suppression des stations sans typologie (pas de calcul possible)
  dplyr::filter(!is.na(TYPO_NATIONALE)) |>
  # Création d'une ligne par année de données
  dplyr::rowwise() |>
  dplyr::mutate(ANNEE = list(seq(PERIODE_DEBUT, PERIODE_FIN))) |>
  dplyr::ungroup() |>
  tidyr::unnest(ANNEE) |>
  dplyr::select(CODE_STATION, TYPO_NATIONALE, TG_BV, PERIODE_DEBUT = ANNEE, PERIODE_FIN = ANNEE)

# Calcul de l'état biologique selon les méthodes 2015 et 2018
# Le calcul de l'état biologique se fait en plusieurs étapes :
# 1. Calcul des EQR (Ecological Quality Ratio) pour chaque indice
# 2. Attribution des classes de qualité selon les seuils spécifiques à chaque type de cours d'eau
#    Les classes sont : Très bon, Bon, Moyen, Médiocre, Mauvais

# Méthode 2018 (EBio_CE_2018 v1.0.1)
# Cette méthode utilise :
# - IBD : avec prise en compte des très grands cours d'eau
# - IBMR : avec seuils par type de cours d'eau
# - I2M2 : avec seuils par type de cours d'eau
# - IPR : avec seuils par type de cours d'eau et altitude

# Méthode 2015 (EBio_CE_2015 v1.0.1)
# Cette méthode utilise :
# - IBG-DCE : avec seuils par type de cours d'eau
# - Invertébrés GCE : avec seuils par type de cours d'eau

# Les algorithmes sont téléchargés depuis le SEEE
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

# Calcul de l'état biologique avec la méthode 2018
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

# Ajout des indices non calculés par le SEEE
# Certains indices ne sont pas calculés par le SEEE car ils ne sont pas utilisés
# dans le calcul de l'état biologique. On les ajoute ici pour avoir une vue
# complète des données disponibles.
etat_bio <- etat_bio |>
  dplyr::bind_rows(
    indices |>
      # Identification des indices non calculés par le SEEE
      dplyr::anti_join(etat_bio, by = c("code_indice", "code_station_hydrobio", "annee")) |>
      dplyr::select(
        code_station_hydrobio, annee, code_indice, libelle_indice, resultat_indice, code_support, libelle_support
      )
  ) |>
  # Harmonisation des libellés des indices
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

# Extraction des paramètres de classification pour chaque indice
# Les paramètres sont stockés dans des fichiers CSV dans les dossiers des algorithmes
# du SEEE. Ils contiennent les valeurs de référence et les seuils de classification
# pour chaque type de cours d'eau.

# Identification des fichiers de paramètres
# Les fichiers sont stockés dans le dossier de l'algorithme EBio_CE_2018 v1.0.1
# et contiennent les paramètres pour chaque indice (IBD, IBMR, I2M2, IPR)
fichiers_parametres <- list.files(
  path = "algo_seee/EBio_CE_2018/1.0.1",
  pattern = "params",
  full.names = TRUE
)

# Ajout des paramètres de l'IBG-DCE de la méthode 2015
# Les paramètres de l'IBG-DCE ne sont pas utilisés dans la méthode 2018
# mais sont nécessaires pour le calcul de l'état biologique avec la méthode 2015
# L'IBG-DCE est remplacé par l'I2M2 dans la méthode 2018 mais reste utilisé
# pour les données historiques
fichiers_parametres <- c(
  fichiers_parametres[!stringr::str_detect(fichiers_parametres, "IBG-DCE")],
  "algo_seee/EBio_CE_2015/1.0.1/EBio_CE_2015_params_IBG-DCE.csv"
)

# Extraction des noms des indices à partir des noms de fichiers
# Les noms des indices sont extraits des noms de fichiers en supprimant
# le chemin du dossier et l'extension .csv
# Exemple : algo_seee/EBio_CE_2018/1.0.1/EBio_CE_2018_params_IBD.csv -> IBD
noms_indices_param <- fichiers_parametres |>
  stringr::str_remove(
    pattern = "algo_seee/EBio_CE_201\\d/1.0.1/EBio_CE_201\\d_params_"
) |>
  stringr::str_remove(
    pattern = ".csv"
  )

# Import et formatage des valeurs seuils pour chaque indice
# Les valeurs seuils sont spécifiques à chaque indice et type de cours d'eau
# Elles permettent de calculer les EQR et d'attribuer les classes de qualité
# Les fichiers CSV utilisent la virgule comme séparateur décimal
# Structure des fichiers :
# - TYPO_NATIONALE : type de cours d'eau
# - TG_BV : présence de très grands bassins versants (>10 000 km²) pour l'IBD
# - REFERENCE : valeur de référence pour le calcul de l'EQR
# - MINIMUM : valeur minimale pour le calcul de l'EQR (IBD uniquement)
# - TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS : seuils de classification
valeurs_seuils <- fichiers_parametres |>
  purrr::map(
    function(x) {
      vroom::vroom(file = x, locale = vroom::locale(decimal_mark = ","))
    }
  ) |>
  purrr::set_names(noms_indices_param)

# Extraction des paramètres nécessaires au calcul des EQR (Ecological Quality Ratio)
# L'EQR est un indicateur normalisé permettant de comparer les résultats entre
# différents types de cours d'eau. 

# Pour chaque indice, on extrait les paramètres nécessaires au calcul :
# - IBD (code 5856) : 
#   * Valeurs de référence et minimales spécifiques à chaque type de cours d'eau
#   * Prise en compte de la présence de très grands bassins versants (>10 000 km²)
# - IBMR (code 2928) : 
#   * Valeurs de référence spécifiques à chaque type de cours d'eau
# - IBG-DCE (code 5910) et Invertébrés GCE (code 6951) : 
#   * Valeurs de référence spécifiques à chaque type de cours d'eau
parametres_eqr <- c("IBD", "IBMR", "IBG-DCE", "MGCE") |>
  purrr::map(
    function(i) {
      if (i == "IBD") {
        # Pour l'IBD, on prend en compte la présence de très grands bassins versants
        params <- valeurs_seuils[[i]] |>
          dplyr::distinct(TYPO_NATIONALE, TG_BV, REFERENCE, MINIMUM)
      }

      if (i %in% c("IBG-DCE", "MGCE", "IBMR")) {
        # Pour les autres indices, seule la typologie et la valeur de référence sont prises en compte
        params <- valeurs_seuils[[i]] |>
          dplyr::distinct(TYPO_NATIONALE, REFERENCE)
      }

      params
    }
  ) |>
  # Association des paramètres avec les codes des indices
  # - 5856 : IBD (Indice Biologique Diatomées)
  # - 2928 : IBMR (Indice Biologique Macrophytique en Rivière)
  # - 5910 : IBG-DCE (Indice Biologique Global)
  # - 6951 : Invertébrés GCE (invertébrés Grands Cours d'Eau)
  purrr::set_names(c(5856, 2928, 5910, 6951))

# Transformation des valeurs seuils pour chaque indice
# Les tableaux de seuils sont transformés pour avoir une structure commune :
# - TYPO_NATIONALE : type de cours d'eau
# - classe : classe de qualité (TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS)
# - seuil_bas et seuil_haut : bornes de la classe de qualité

# Le traitement est différent selon l'indice :
# - IPR : les seuils sont des valeurs brutes, plus la valeur est élevée,
#         plus la qualité est mauvaise
# - Autres indices : les seuils sont des EQR, plus la valeur est élevée,
#                   plus la qualité est bonne
for (x in noms_indices_param) {
  print(x)
  if (x == "IPR") {
    # Pour l'IPR, les seuils sont des valeurs brutes
    # Les seuils sont différents selon l'altitude mais seuls les seuils
    # de basse altitude sont utilisés en Île-de-France
    valeurs_seuils$IPR <- valeurs_seuils$IPR |>
      dplyr::mutate(
        # Utilisation des seuils de basse altitude si les seuils
        # ne sont pas spécifiés pour le type de cours d'eau
        bon = ifelse(is.na(BON), BASSE_ALTITUDE, BON)
      ) |>
      dplyr::select(TYPO_NATIONALE, TRES_BON, BON = bon, MOYEN, MEDIOCRE, MAUVAIS) |>
      # Transformation en format long pour avoir une ligne par classe
      tidyr::pivot_longer(
        cols = -TYPO_NATIONALE,
        names_to = "classe", values_to = "seuil_haut"
      ) |>
      # Complète les bornes basses des classes
      dplyr::mutate(
        seuil_bas = ifelse(classe == "TRES_BON", 0, dplyr::lag(seuil_haut))
      )
  } else {
    if (x == "IBD") {
      # Pour l'IBD, les seuils sont des EQR et dépendent de la présence
      # de très grands bassins versants
      valeurs_seuils$IBD <- valeurs_seuils$IBD |>
        dplyr::select(TYPO_NATIONALE, TG_BV, TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS) |>
        # Transformation en format long pour avoir une ligne par classe
        tidyr::pivot_longer(
          cols = -c(TYPO_NATIONALE, TG_BV),
          names_to = "classe", values_to = "seuil_bas"
        ) |>
        # Complète les bornes hautes des classes
        dplyr::mutate(
          seuil_haut = ifelse(classe == "TRES_BON", 1, dplyr::lag(seuil_bas))
        )
    } else {
      # Pour les autres indices (IBMR, IBG-DCE, MGCE), les seuils sont des EQR
      # et ne dépendent que du type de cours d'eau
      valeurs_seuils[[x]] <- valeurs_seuils[[x]] |>
        dplyr::select(TYPO_NATIONALE, TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS) |>
        # Transformation en format long pour avoir une ligne par classe
        tidyr::pivot_longer(
          cols = -c(TYPO_NATIONALE),
          names_to = "classe", values_to = "seuil_bas"
        ) |>
        # Complète les bornes hautes des classes
        dplyr::mutate(
          seuil_haut = ifelse(classe == "TRES_BON", 1, dplyr::lag(seuil_bas))
        )
    }
  }
}

# Association des valeurs seuils avec les stations
# Pour chaque station, on associe les valeurs seuils correspondant à son type
# de cours d'eau et à la présence de très grands bassins versants

# Traitement spécifique pour l'IBD qui dépend de la présence de très grands
# bassins versants (TG_BV)
valeurs_seuils_stations <- stations_seee |>
  sf::st_drop_geometry() |>
  dplyr::distinct(CODE_STATION, TYPO_NATIONALE, TG_BV) |>
  dplyr::mutate(indice = "IBD") |>
  dplyr::left_join(
    valeurs_seuils$IBD,
    by = c("TYPO_NATIONALE", "TG_BV")
  )

# Ajout des valeurs seuils pour les autres indices
# Pour les autres indices (IBMR, IBG-DCE, MGCE, IPR), les seuils ne dépendent
# que du type de cours d'eau
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

# Correction des seuils hauts
# Si le seuil bas n'est pas disponible (NA), le seuil haut est également mis à NA
valeurs_seuils_stations <- valeurs_seuils_stations |>
  dplyr::mutate(
    seuil_haut = ifelse(is.na(seuil_bas), NA, seuil_haut)
  )

# Téléchargement et traitement des listes taxonomiques
# Pour chaque station, on récupère la liste des taxons observés avec leur
# abondance et leur date d'observation

# Les listes sont simplifiées en supprimant les informations sur les variétés,
# les formes anormales, les formes et les sous-espèces pour ne garder que
# l'espèce
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

# Définition des acronymes des indices
# Les codes des indices sont remplacés par leurs acronymes pour une meilleure
# lisibilité dans l'application
acronymes_indices <- c(
  `5856` = "IBD",    # Indice Biologique Diatomées
  `2928` = "IBMR",   # Indice Biologique Macrophytique en Rivière
  `7613` = "I2M2",   # Indice Invertébrés Multi-Métrique
  `5910` = "IBG équivalent",  # Indice Biologique Global
  `6951` = "Invertébrés GCE", # Indice invertébrés Grands Cours d'Eau
  `7036` = "IPR"     # Indice Poissons Rivière
)

# Préparation des données pour l'affichage cartographique
# Pour chaque station, on calcule :
# - La dernière année de prélèvement
# - Les derniers résultats des indices biologiques
# - Le nombre d'années de suivi

# Les résultats sont formatés pour l'affichage avec :
# - Les acronymes des indices à la place des codes
# - La concaténation des résultats des différents indices
# - L'ajout de l'année entre parenthèses
donnees_carte <- stations |>
  dplyr::left_join(
    indices |>
      # Calcul de l'année de prélèvement
      dplyr::mutate(
        annee = lubridate::year(date_prelevement)
      ) |>
      # Conversion des codes d'indices en facteurs pour garantir l'ordre
      dplyr::mutate(
        code_indice = code_indice |>
          factor(levels = names(acronymes_indices))
        ) |>
      # Calcul des statistiques par station et support
      dplyr::group_by(code_station_hydrobio, code_support, libelle_support) |>
      dplyr::reframe(
        derniere_annee = max(annee),
        dernier_indice = code_indice[annee == max(annee)],
        dernier_resultat = resultat_indice[annee == max(annee)],
        nb_annees = dplyr::n_distinct(annee)
        ) |>
      # Tri des résultats par station, support et indice
      dplyr::arrange(code_station_hydrobio, libelle_support, dernier_indice) |>
      # Formatage des résultats avec les acronymes
      dplyr::mutate(
        dernier_resultat = paste0(acronymes_indices[as.character(dernier_indice)],  ": ", dernier_resultat)
      ) |>
      dplyr::select(-dernier_indice) |>
      # Regroupement des résultats par station
      dplyr::group_by(code_station_hydrobio, code_support, libelle_support, derniere_annee, nb_annees) |>
      dplyr::summarise(
        derniers_resultats = paste(dernier_resultat, collapse = "/"),
        .groups = "drop"
      ) |>
      # Ajout de l'année entre parenthèses
      dplyr::mutate(
        derniers_resultats = paste0(derniers_resultats, " (", derniere_annee, ")")
        ),
    by = "code_station_hydrobio",
    multiple ="all"
  ) |>
  # Suppression des stations sans support biologique
  dplyr::filter(!is.na(libelle_support)) |>
  # Transformation en WGS84 pour l'affichage cartographique
  sf::st_transform(crs = 4326)

# Préparation des données taxonomiques pour l'affichage cartographique
# Pour chaque taxon et station, on calcule :
# - La dernière date d'observation
# - L'abondance moyenne
# - Un résumé des abondances et des années d'observation

# Les résultats sont formatés en HTML pour l'affichage au survol
# avec le nom du taxon en gras, le nom de la station en italique
# et le résumé des observations
donnees_carte_taxons <-
  dplyr::left_join(
    stations |>
      dplyr::select(code_departement, code_station_hydrobio),
    listes_taxo |>
      # Calcul des statistiques par taxon et station
      dplyr::group_by(code_station_hydrobio, libelle_station_hydrobio, code_support, libelle_taxon) |>
      dplyr::summarise(
        derniers_resultats = max(date_prelevement),
        ab_moy = mean(resultat_taxon),
        # Création d'un résumé avec les abondances minimales et maximales
        # et les années de première et dernière observation
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
      # Formatage du texte au survol en HTML
      dplyr::mutate(
        hover = paste0(
          "<b>", libelle_taxon, "</b><br>",
          "<em>", libelle_station_hydrobio, "</em><br><br>",
          resume
        )
      ),
    by = "code_station_hydrobio", multiple = "all"
  ) |>
  # Transformation en WGS84 pour l'affichage cartographique
  sf::st_transform(crs = 4326)

resumes_listes <- HydrobioIdF::resumer_listes(listes_taxo)


# Sauvegarde des données préparées
# Les objets suivants sont sauvegardés :
# - date_donnees : date de mise à jour des données
# - regie : stations suivies en régie
# - stations : stations hydrobiologiques avec leurs coordonnées et attributs
# - indices : indices biologiques bruts
# - valeurs_seuils_stations : seuils de classification par station et indice
# - etat_bio : état biologique calculé selon les méthodes 2015 et 2018
# - listes_taxo : listes taxonomiques brutes
# - resumes_listes : résumés des listes taxonomiques
# - acronymes_indices : correspondance entre codes et acronymes des indices
# - donnees_carte : données formatées pour l'affichage cartographique
# - donnees_carte_taxons : données taxonomiques pour l'affichage cartographique
# - parametres_eqr : paramètres de calcul des EQR par indice
save(date_donnees, regie, stations, indices, valeurs_seuils_stations, etat_bio, 
     listes_taxo, resumes_listes, acronymes_indices, donnees_carte, 
     donnees_carte_taxons, parametres_eqr,
     file = "dev/data_hydrobio.rda")



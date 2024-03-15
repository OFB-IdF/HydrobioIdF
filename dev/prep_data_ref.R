limites_region <-  sf::st_read("C:/QGIS-CUSTOM/DATA/VECTEUR/administration/ADMIN EXPRESS/ADE_3-1_SHP_LAMB93_FXX/DEPARTEMENT.shp") %>%
  dplyr::filter(INSEE_REG == "11") %>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

limites_region_l <- limites_region %>%
  sf::st_cast(to = "LINESTRING")

limites_bassin <- sf::st_read(
  dsn = "C:/QGIS-CUSTOM/DATA/VECTEUR/hydrographie/bdtopage_idf.gpkg",
  layer = "BassinHydrographique"
) %>%
  dplyr::filter(LbBH == "Seine-Normandie") %>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

limites_bassin_l <- limites_bassin %>%
  sf::st_cast(to = "LINESTRING")

masque_metropole <- sf::st_read("C:/QGIS-CUSTOM/DATA/VECTEUR/administration/ADMIN EXPRESS/ADE_3-1_SHP_LAMB93_FXX/REGION.shp") %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_difference(limites_bassin) %>%
  # dplyr::filter(INSEE_REG != "11") %>%
  dplyr::summarise() %>%
  rmapshaper::ms_simplify()

edl <- sf::st_read("C:/QGIS-CUSTOM/DATA/VECTEUR/surveillance/edl_sn.gpkg") %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  dplyr::mutate(
    dplyr::across(
      c("ETAT.BIOLOGIQUE", "ETAT.ECOLOGIQUE", "ETAT.PHYSICO.CHIMIQUE"),
      function(x) {factor(x, levels = c("très bon", "bon", "moyen", "médiocre", "mauvais", "indéterminé"))}
    ),
    ANNEE = 2022
  ) %>%
  rmapshaper::ms_simplify()

regie <- HydrobioIdF::importer_suivis_regie("dev/Historique prog labo.xlsx")

usethis::use_data(
  limites_region, limites_region_l,
  limites_bassin, limites_bassin_l,
  masque_metropole,
  edl, regie,
  internal = TRUE, overwrite = TRUE
  )

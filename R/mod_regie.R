#' checkbox UI Function
#'
#' @description Module Shiny permettant de filtrer les stations selon leur mode de gestion (régie/prestataire).
#' Le module affiche une case à cocher permettant de sélectionner les stations en régie ou non.
#'
#' @param id Internal parameter for {shiny}.
#' @param titre Texte à afficher à côté de la case à cocher.
#' @param defaut Valeur booléenne indiquant si la case doit être cochée par défaut.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return Un objet tagList contenant une case à cocher.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regie_ui <- function(id, titre = "", defaut = FALSE){
  ns <- NS(id)
  tagList(
    checkboxInput(
      inputId = ns("checkbox"),
      label = titre,
      value = defaut
    )
  )
}

#' checkbox Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param choix_eqbs Fonction réactive retournant les codes des éléments de qualité sélectionnés.
#' @param choix_dep Fonction réactive retournant les codes des départements sélectionnés.
#'
#' @details La fonction serveur filtre les stations selon leur mode de gestion (régie/prestataire)
#' en fonction des éléments de qualité et départements sélectionnés.
#'
#' @return Une fonction réactive retournant un vecteur de codes stations.
#'
#' @noRd
mod_regie_server <- function(id, choix_eqbs, choix_dep){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive({
      req(choix_eqbs, choix_dep)

      eqbs <- choix_eqbs()
      if (is.null(eqbs))
        eqbs <- unique(donnees_carte$code_support)

      deps <- choix_dep()
      if (is.null(deps))
        deps <- unique(donnees_carte$code_departement)
      if ("PPC" %in% deps)
        deps <- c(deps[deps != "PPC"], 75, 92, 93, 94)

      liste_stations <- indices %>%
        dplyr::left_join(
          stations %>%
            sf::st_drop_geometry() %>%
            dplyr::select(code_station_hydrobio, code_departement),
          by = "code_station_hydrobio"
        ) %>%
        dplyr::distinct(code_departement, code_station_hydrobio, code_support) %>%
        dplyr::filter(
          code_support %in% eqbs,
          code_departement %in% deps
        ) %>%
        dplyr::left_join(
          regie %>%
            dplyr::distinct(code_station, indice) %>%
            dplyr::mutate(
              code_support = c("IBD" = "10",
                               "MPCE" = "13",
                               "IBMR" = "27")[indice],
              regie = TRUE
            ) %>%
            dplyr::select(code_station_hydrobio = code_station, code_support, regie),
          by = c("code_station_hydrobio", "code_support")
        ) %>%
        dplyr::mutate(regie = ifelse(is.na(regie), FALSE, regie))

      if (input$checkbox) {
        liste_stations <- liste_stations %>%
          dplyr::filter(regie)
      }

      unique(liste_stations$code_station_hydrobio)
    })

  })
}

## To be copied in the UI
# mod_checkbox_ui("checkbox_1")

## To be copied in the server
# mod_checkbox_server("checkbox_1")

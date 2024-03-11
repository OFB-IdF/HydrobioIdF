#' repartition_taxons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_repartition_taxons_ui <- function(id){
  ns <- NS(id)

  css <- HTML(
    paste0(
      paste0("#", ns("carte_taxon"), " {margin-bottom:10px !important;height: calc(100vh - 200px) !important;}"),
      ".search-taxon {
            position: absolute;
            top: -5px;
            left: 100px;
          }

           .leaflet {
                margin-top:0px;
                padding:0px;
           }

           .leaflet-control-zoom, .leaflet-top, .leaflet-bottom {
           z-index: unset !important;
           }

           .leaflet-touch .leaflet-control-layers .leaflet-control-zoom .leaflet-touch .leaflet-bar {
           z-index: 10000000000 !important;
           }
          "
    )
  )

  tagList(
    tags$head(
      tags$style(css)
    ),
    column(
      width = 12,
      tags$div(
        class = "search-taxon",
        selectizeInput(
          inputId = ns("taxon"),
          label = "",
          choices = c(
            "Choisir un taxon" = ""
          ),
          multiple = FALSE
        )
      ),
      leaflet::leafletOutput(
        ns("carte_taxon"),
        width = '100%'
      )
    )

  )
}

#' repartition_taxons Server Functions
#'
#' @noRd
#' @importFrom dplyr mutate select filter pull
#' @importFrom htmltools HTML
#' @importFrom leaflet renderLeaflet leaflet addMapPane addTiles WMSTileOptions providerTileOptions addPolygons pathOptions addWMSTiles addPolylines addLayersControl layersControlOptions fitBounds leafletProxy clearMarkers addCircleMarkers
#' @importFrom leaflet.extras addResetMapButton
#' @importFrom sf st_bbox
#' @importFrom shiny HTML
mod_repartition_taxons_server <- function(id, listes, departements, eqb, suivi_regie){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # radius_pal <- function(x) {
    #   approx(
    #     x = sqrt(range(listes$ab_moyen, na.rm = TRUE)),
    #     y = c(5, 10),
    #     xout = sqrt(x),
    #     yleft = 5,
    #     yright = 10
    #   )$y
    # }

    BboxMap <- sf::st_bbox(listes)

    couleurs_etat <- c(
      `indéterminé` = "#CDC0B0",
      mauvais = "#EE2C2C",
      `médiocre` = "#FF7F00",
      moyen = "#FFC125",
      bon = "#A2CD5A",
      `très bon` = "#1874CD"
    )

    output$carte_taxon <- leaflet::renderLeaflet(
      leaflet::leaflet() %>%
        leaflet::addMapPane("background", zIndex = 400) %>%
        leaflet::addMapPane("masks", zIndex = 450) %>%
        leaflet::addMapPane("foreground", zIndex = 500) %>%
        leaflet::addTiles(map = .) %>%
        leaflet::addTiles(group = "OSM") %>%
        leaflet::addTiles("http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
                          options = c(leaflet::WMSTileOptions(tileSize = 256),
                                      leaflet::providerTileOptions(minZoom = 1, maxZoom = 15)),
                          attribution='<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
                          group = "Plan IGN"
        ) %>%
        leaflet::addTiles("http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
                          options = c(leaflet::WMSTileOptions(tileSize = 256),
                                      leaflet::providerTileOptions(minZoom = 1, maxZoom = 22)),
                          attribution='<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
                          group = "Photo aérienne"
        ) %>%
        leaflet::addPolygons(
          data = edl %>%
            dplyr::mutate(
              LABEL = paste0(NOM.MASSE.D.EAU, "<br>", ETAT.BIOLOGIQUE, " (", ANNEE, ")")
            ) %>%
            dplyr::select(LABEL, ETAT.BIOLOGIQUE),
          group = "Etat biologique",
          fillColor = ~unname(couleurs_etat[as.character(ETAT.BIOLOGIQUE)]),
          fillOpacity = .5,
          label = ~lapply(LABEL, htmltools::HTML),
          popup = NULL,
          weight = 1,
          options = leaflet::pathOptions(pane = "background")
        ) %>%
        leaflet::addPolygons(
          data = edl %>%
            dplyr::mutate(
              LABEL = paste0(NOM.MASSE.D.EAU, "<br>", ETAT.ECOLOGIQUE, " (", ANNEE, ")")
            ) %>%
            dplyr::select(LABEL, ETAT.ECOLOGIQUE),
          group = "Etat écologique",
          fillColor = ~unname(couleurs_etat[as.character(ETAT.ECOLOGIQUE)]),
          fillOpacity = .5,
          label = ~lapply(LABEL, htmltools::HTML),
          popup = NULL,
          weight = 1,
          options = leaflet::pathOptions(pane = "background")
        ) %>%
        leaflet::addPolygons(
          data = edl %>%
            dplyr::mutate(
              LABEL = paste0(NOM.MASSE.D.EAU, "<br>", ETAT.PHYSICO.CHIMIQUE, " (", ANNEE, ")")
            ) %>%
            dplyr::select(LABEL, ETAT.PHYSICO.CHIMIQUE),
          group = "Etat physico-chimique",
          fillColor = ~unname(couleurs_etat[as.character(ETAT.PHYSICO.CHIMIQUE)]),
          fillOpacity = .5,
          label = ~lapply(LABEL, htmltools::HTML),
          popup = NULL,
          weight = 1,
          options = leaflet::pathOptions(pane = "background")
        ) %>%
        leaflet::addWMSTiles(
          baseUrl = "https://services.sandre.eaufrance.fr/geo/topage",
          layers = "CoursEau_FXX",
          group = "Réseau hydrographique",
          options = leaflet::WMSTileOptions(
            pane = "masks",
            format = "image/png",
            transparent = TRUE,
            crs = 4326
          )
        ) %>%
        # leaflet::addPolylines(
        #   data = reseau_hydro,
        #   group = "Réseau hydrographique",
        #   color = "#00B2EE",
        #   weight = 1,
        #   label = ~TopoOH,
        #   popup = NULL,
        #   opacity = 1,
        #   options = leaflet::pathOptions(pane = "masks")
        # ) %>%
        leaflet::addPolygons(
          data = masque_metropole,
          fillColor = "white",
          fillOpacity = .75,
          stroke = FALSE,
          popup = NULL, label = NULL,
          options = leaflet::pathOptions(pane = "masks")
        ) %>%
        leaflet::addPolylines(
          data = limites_bassin_l,
          color = "black",
          opacity = 1,
          weight = 2,
          options = leaflet::pathOptions(pane = "masks")
        ) %>%
        leaflet::addPolylines(
          data = limites_region_l,
          color = "black",
          opacity = 1,
          weight = 2,
          options = leaflet::pathOptions(pane = "masks")
        ) %>%
        leaflet::addLayersControl(
          baseGroups    = c("OSM","Plan IGN","Photo aérienne",  "Etat écologique","Etat biologique", "Etat physico-chimique"),
          overlayGroups = c("Réseau hydrographique", "Indices"),
          options       = leaflet::layersControlOptions(collapsed = TRUE)) %>%
        leaflet::fitBounds(
          map = .,
          lng1 = BboxMap[["xmin"]],
          lat1 = BboxMap[["ymin"]],
          lng2 = BboxMap[["xmax"]],
          lat2 = BboxMap[["ymax"]]
        ) %>%
        leaflet.extras::addResetMapButton()
    )

    observe({
      req(eqb)

      choix_eqb <- eqb()
      if (is.null(choix_eqb))
        choix_eqb <- unique(listes$code_support)

      updateSelectizeInput(
        session = session,
        inputId = "taxon",
        choices = c(
          "Choisir un taxon" = "",
          listes %>%
            dplyr::filter(code_support %in% choix_eqb) %>%
            dplyr::pull(libelle_taxon) %>%
            unique()
        ),
        server = TRUE
      )

    })
    observe({
      req(listes, departements, input$taxon, suivi_regie)

      deps <- departements()
      if (is.null(deps))
        deps <- unique(listes$code_departement)
      if ("PPC" %in% deps)
        deps <- c(deps[deps != "PPC"], 75, 92, 93, 94)

      # if (suivi_regie())
      #   DonneesCarte <- DonneesCarte %>%
      #   dplyr::filter(regie & choix_eqb != 4)

      DonneesCarte <- listes %>%
        dplyr::filter(
          code_departement %in% deps,
          libelle_taxon == input$taxon
        )

      print(colnames(DonneesCarte))

      BboxMap <- sf::st_bbox(
        DonneesCarte %>%
          dplyr::filter(libelle_taxon == input$taxon)
        )

      leaflet::leafletProxy("carte_taxon") %>%
        leaflet::fitBounds(
          map = .,
          lng1 = BboxMap[["xmin"]],
          lat1 = BboxMap[["ymin"]],
          lng2 = BboxMap[["xmax"]],
          lat2 = BboxMap[["ymax"]]
        )


      if (nrow(DonneesCarte %>% dplyr::filter(libelle_taxon == input$taxon)) == 0) {
        leaflet::leafletProxy("carte_taxon") %>%
          leaflet::clearMarkers(map = .)
      } else {
        leaflet::leafletProxy("carte_taxon") %>%
          leaflet::clearMarkers(map = .) %>%
          leaflet::addCircleMarkers(
            map = .,
            data = DonneesCarte %>%
              dplyr::filter(libelle_taxon == input$taxon),
            layerId = ~code_station_hydrobio,
            # radius = ~radius_pal(ab_moyen),
            radius = 7,
            stroke = TRUE,
            color = "black",
            fillColor = c("#FFB90F"),
            fillOpacity = 1,
            weight = 2,
            label = ~lapply(hover, shiny::HTML),
            options = pathOptions(pane = "foreground")
          )
      }


    })


    reactive(input$taxon)
  })
}

## To be copied in the UI
# mod_repartition_taxons_ui("repartition_taxons_1")

## To be copied in the server
# mod_repartition_taxons_server("repartition_taxons_1")

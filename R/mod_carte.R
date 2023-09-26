#' carte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_carte_ui <- function(id){
  ns <- NS(id)

  css <- HTML(
    paste0(
      paste0("#", ns("carte_op"), " {height: calc(100vh - 200px) !important;}"),
      ".search-station {
            position: absolute;
            top: 0px;
            left: 80px;
          }

           .leaflet {
                margin-top:0px;
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
        class = "search-station",
        selectizeInput(
          inputId = ns("station"),
          label = "",
          # choices = c(
          #     "Localiser une station" = "",
          #     sort(unique(pop_geo$pop_libelle))
          # ),
          choices = c(
            "Localiser un point de prélèvement" = ""
          ),
          multiple = FALSE
        )
      ),
      leaflet::leafletOutput(
        ns("carte_op"),
        width = '100%'
      )
    )

  )
}

#' carte Server Functions
#'
#' @noRd
mod_carte_server <- function(id, limites){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    BboxMap <- sf::st_bbox(limites)

    couleurs_etat <- c(
      `indéterminé` = "#CDC0B0",
      mauvais = "#EE2C2C",
      `médiocre` = "#FF7F00",
      moyen = "#FFC125",
      bon = "#A2CD5A",
      `très bon` = "#1874CD"
    )

    output$carte_op <- leaflet::renderLeaflet(
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
        leaflet::addPolylines(
          data = reseau_hydro,
          group = "Réseau hydrographique",
          color = "#00B2EE",
          weight = 1,
          label = ~TopoOH,
          popup = NULL,
          opacity = 1,
          options = leaflet::pathOptions(pane = "masks")
        ) %>%
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
        )
    )

  })
}

## To be copied in the UI
# mod_carte_ui("carte_1")

## To be copied in the server
# mod_carte_server("carte_1")

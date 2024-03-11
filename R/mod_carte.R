#' carte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_carte_ui <- function(id, hauteur){
  ns <- NS(id)

  css <- HTML(
    paste0(
      paste0("#", ns("carte_op"), " {margin-bottom:10px !important;}"),
      ".search-station {
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
        class = "search-station",
        selectizeInput(
          inputId = ns("station"),
          label = "",
          choices = c(
            "Localiser une station" = ""
          ),
          multiple = FALSE
        )
      ),
      leaflet::leafletOutput(
        ns("carte_op"),
        width = '100%',
        height = hauteur
      )
    )

  )
}

#' carte Server Functions
#'
#' @noRd
#' @importFrom dplyr mutate select
#' @importFrom htmltools HTML
#' @importFrom leaflet renderLeaflet leaflet addMapPane addTiles WMSTileOptions providerTileOptions addPolygons pathOptions addPolylines addLayersControl layersControlOptions fitBounds
#' @importFrom leaflet.extras addResetMapButton
#' @importFrom sf st_bbox
#' @importFrom dplyr `%>%`
mod_carte_server <- function(id, donnees_carte, departements, eqb, suivi_regie){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    radius_pal <- function(x) {
      approx(
        x = sqrt(range(donnees_carte$nb_annees, na.rm = TRUE)),
        y = c(5, 10),
        xout = sqrt(x),
        yleft = 5,
        yright = 10
      )$y
    }

    BboxMap <- sf::st_bbox(donnees_carte)

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
      req(departements, eqb, suivi_regie)

      deps <- departements()
      if (is.null(deps))
        deps <- unique(donnees_carte$code_departement)
      if ("PPC" %in% deps)
        deps <- c(deps[deps != "PPC"], 75, 92, 93, 94)

      choix_eqb <- eqb()
      if (is.null(choix_eqb))
        choix_eqb <- unique(donnees_carte$code_support)

      DonneesCarte <- donnees_carte %>%
        dplyr::filter(
          code_departement %in% deps,
          code_support %in% choix_eqb
          )

      if (suivi_regie())
        DonneesCarte <- DonneesCarte %>%
        dplyr::filter(regie & choix_eqb != 4)

      DonneesCarte <- DonneesCarte %>%
        dplyr::group_by(code_station_hydrobio, libelle_station_hydrobio) %>%
        dplyr::summarise(
          derniers_resultats = paste(derniers_resultats, collapse = "<br>"),
          nb_annees = max(nb_annees),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          hover = paste0(
            "<b>", libelle_station_hydrobio, "</b><br><br>",
            derniers_resultats
          )
        )

      updateSelectizeInput(
        session = session,
        inputId = "station",
        choices = c(
          "Localiser une station" = "",
          DonneesCarte$libelle_station_hydrobio
        ),
        server = TRUE
      )

      BboxMap <- sf::st_bbox(DonneesCarte)

      leaflet::leafletProxy("carte_op") %>%
        leaflet::fitBounds(
          map = .,
          lng1 = BboxMap[["xmin"]],
          lat1 = BboxMap[["ymin"]],
          lng2 = BboxMap[["xmax"]],
          lat2 = BboxMap[["ymax"]]
        )


      if (nrow(DonneesCarte) == 0) {
        leaflet::leafletProxy("carte_op") %>%
          leaflet::clearMarkers(map = .)
      } else {
        leaflet::leafletProxy("carte_op") %>%
          leaflet::clearMarkers(map = .) %>%
          leaflet::addCircleMarkers(
            map = .,
            data = DonneesCarte,
            layerId = ~code_station_hydrobio,
            radius = ~radius_pal(nb_annees),
            stroke = TRUE,
            color = "black",
            fillColor = "white",
            fillOpacity = 1,
            weight = 2,
            label = ~lapply(hover, shiny::HTML),
            options = pathOptions(pane = "foreground")
          )
      }

      observe({

        if (input$station != "") {

          CoordsStation <- DonneesCarte %>%
            dplyr::filter(libelle_station_hydrobio == input$station) %>%
            dplyr::summarise() %>%
            sf::st_centroid() %>%
            sf::st_coordinates()

          leaflet::leafletProxy("carte_op") %>%
            leaflet::setView(
              lng = unname(CoordsStation[,"X"]),
              lat = unname(CoordsStation[,"Y"]),
              zoom = 15
            )
        } else {

          leaflet::leafletProxy("carte_op") %>%
            leaflet::fitBounds(
              map = .,
              lng1 = BboxMap[["xmin"]],
              lat1 = BboxMap[["ymin"]],
              lng2 = BboxMap[["xmax"]],
              lat2 = BboxMap[["ymax"]]
            )
        }

      })


    })

    SelectionPoint <- reactiveValues(clickedMarker=NULL)

    # observe the marker click info and print to console when it is changed.
    observeEvent(input$carte_op_marker_click,{
      SelectionPoint$clickedMarker <- input$carte_op_marker_click$id
    })

    # POUR UNE RAISON QUE JE NE COMPRENDS PAS
    # CELA NE FONCTIONNE PAS
    # REINITIALISE LA VALEUR AU CLIC MEME SUR MARQUEUR ET
    # PAS QUE SUR FOND DE CARTE
    # observeEvent(input$carte_op_click,{
    #   SelectionPoint$clickedMarker <- NULL
    #   print("reset")
    # })


    reactive(SelectionPoint$clickedMarker)
  })
}

## To be copied in the UI
# mod_carte_ui("carte_1")

## To be copied in the server
# mod_carte_server("carte_1")

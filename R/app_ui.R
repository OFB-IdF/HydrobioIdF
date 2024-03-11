#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        fluidPage(
            div(
                h1(
                    class = "TitreAppli",
                    "Suivis hydrobiologiques en Île-de-France"
                ),
                div(
                  div("Date d'accès aux données:"),
                  div(date_donnees),
                  style = "position: absolute; bottom: 0, width: 10%;"
                ),
                img(
                    src = knitr::image_uri(
                        app_sys("app/www/logo.png")
                    ),
                    alt = 'logo',
                    style = '
                    position:fixed;
                    bottom:0;
                    right:0;
                    padding:10px;
                    width:200px;
                    '
                ),
                img(
                    src = knitr::image_uri(
                        app_sys("app/www/filigrane.png")
                    ),
                    alt = "filigrane",
                    style = '
                    position:fixed;
                    bottom:0;
                    right:0;
                    padding:0px;
                    width:800px;
                    color:rgb(153, 215, 247);
                    '
                    ##99D7F7;
                )
            ),
            sidebarLayout(
                sidebarPanel = sidebarPanel(
                    width = 2,
                    h2("Panneau de sélection"),
                    mod_selecteur_ui(
                      id = "departements",
                      titre = "Départements",
                      texte = "Tous",
                      choix = c(
                        `Essonne` = 91,
                        `Seine-et-Marne` = 77,
                        `Val-d'Oise` = 95,
                        `Yvelines` = 78,
                        `Paris et Petite Couronne` = "PPC"
                        ),
                      choix_multiple = TRUE
                    ),
                    mod_selecteur_ui(
                      id = "eqb",
                      titre = "Eléments de qualité biologique",
                      texte = "Tous",
                      choix = c(
                        "Diatomées" = 10,
                        "Macrophytes" = 27,
                        "Macroinvertébrés" = 13,
                        "Poissons" = 4
                      ),
                      choix_multiple = TRUE
                    ),
                    mod_checkbox_ui(
                      id = "regie",
                      titre = "Suivis en régie"
                    )
                ),
                mainPanel = mainPanel(
                    width = 10,
                    tabsetPanel(
                        tabPanel(
                            title = "Communautés",
                            fluidRow(
                              column(
                                width = 6,
                                  mod_carte_ui(
                                  id = "carte",
                                  hauteur = "500px"
                                ),
                                mod_synthese_toutes_stations_ui(
                                  id = "bilan_stations"
                                )
                              ),
                              column(
                                width = 6,
                                mod_synthese_station_ui(id = "synthese_station")
                              )
                            )
                        ),
                        tabPanel(
                            title = "Taxons",
                            fluidRow(
                              column(
                                width = 6,
                                mod_repartition_taxons_ui(id = "carte_taxons")
                              ),
                              column(
                                width = 6,
                                mod_synthese_taxon_ui(id = "synthese_taxon")
                              )
                            )
                        ),
                        tabPanel(
                            title = p(
                                class = "TabMethode",
                                "Données & Traitements"
                            )
                        )
                    )
                )
            )
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

    add_resource_path(
        'www', app_sys('app/www')
    )

    tags$head(
        favicon(
            ico = "favicon",
            ext = "png"
        ),
        bundle_resources(
            path = app_sys('app/www'),
            app_title = 'Hydrobio IdF'
        )
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert()
    )
}


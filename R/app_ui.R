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
                    "Titre du tableau de bord"
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
                    width = 3,
                    h2("Panneau de sélection")
                ),
                mainPanel = mainPanel(
                    width = 9,
                    tabsetPanel(
                        tabPanel(
                            title = "Onglet 1",
                            tags$div(
                                class = "sub-tabpanel",
                                tabsetPanel(
                                    tabPanel(
                                        title = "Sous-onglet 1"
                                    ),
                                    tabPanel(
                                        title = "Sous-onglet 2"
                                    )
                                )
                            )
                        ),
                        tabPanel(
                            title = "Onglet 2"
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
            app_title = 'Titre de l\'appli'
        )
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert()
    )
}


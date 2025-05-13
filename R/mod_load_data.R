#' load_data UI Function
#'
#' @description Module Shiny permettant de charger les données hydrobiologiques depuis un fichier distant.
#' Le module télécharge les données depuis GitHub, les charge dans l'environnement global et affiche
#' la date de mise à jour des données.
#'
#' @param id Internal parameter for {shiny}.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return Un objet tagList contenant un texte avec la date de mise à jour des données.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("date"))
  )
}

#' load_data Server Functions
#'
#' @param id Internal parameter for {shiny}.
#'
#' @details La fonction serveur télécharge le fichier 'data_hydrobio.rda' depuis le dépôt GitHub,
#' charge les données dans l'environnement global et supprime le fichier temporaire.
#' La date de mise à jour des données est affichée via l'objet 'date_donnees'.
#'
#' @return La date de mise à jour des données sous forme de texte.
#'
#' @noRd
mod_load_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    download.file(url = "https://github.com/OFB-IdF/HydrobioIdF/raw/main/dev/data_hydrobio.rda", destfile = "data_hydrobio.rda")
    load("data_hydrobio.rda", envir = .GlobalEnv)
    unlink("data_hydrobio.rda")

    output$date <- renderText(as.character(get("date_donnees")))

  })
}

## To be copied in the UI
# mod_load_data_ui("load_data_1")

## To be copied in the server
# mod_load_data_server("load_data_1")

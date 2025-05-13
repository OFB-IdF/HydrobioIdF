#' Tracer les chroniques d'abondance des taxons
#'
#' @param liste_station Un data.frame contenant les listes faunistiques ou floristiques
#'   avec les colonnes date_prelevement, libelle_taxon et resultat_taxon
#' @param ordre_taxon Une chaîne de caractères indiquant l'ordre de présentation des taxons :
#'   "ordre alphabétique", "abondance sur la chronique" ou une année spécifique
#'
#' @return Une liste d'objets ggplot2 représentant les chroniques d'abondance des taxons
#'   par groupes de 20 taxons maximum
#' @export
#'
#' @details Cette fonction trace les chroniques d'abondance des taxons en les regroupant
#'   par lots de 20 taxons maximum. L'ordre des taxons peut être :
#'   \itemize{
#'     \item alphabétique (ordre inverse)
#'     \item selon l'abondance totale sur la chronique
#'     \item selon l'abondance pour une année spécifique
#'   }
#'
#' @examples
#' \dontrun{
#' chroniques <- tracer_chroniques_taxons(liste_station, "ordre alphabétique")
#' chroniques <- tracer_chroniques_taxons(liste_station, "abondance sur la chronique")
#' chroniques <- tracer_chroniques_taxons(liste_station, 2020)
#' }
#' @importFrom dplyr mutate group_by summarise group_split
#' @importFrom ggplot2 ggplot geom_point aes scale_x_continuous sec_axis theme_minimal theme element_blank element_text labs
#' @importFrom lubridate year
#' @importFrom purrr map
tracer_chroniques_taxons <- function(liste_station, ordre_taxon) {
  liste_station <- liste_station %>%
    dplyr::mutate(annee = lubridate::year(date_prelevement))

  x_lims <- range(na.omit(liste_station$annee))
  x_breaks <- integer_breaks(n = 3)(liste_station$annee)

  if (ordre_taxon == "ordre alphabétique") {
    taxons <- liste_station %>%
      dplyr::distinct(libelle_taxon) %>%
      dplyr::arrange(dplyr::desc(libelle_taxon)) %>%
      dplyr::mutate(
        taxon = libelle_taxon %>%
          forcats::fct_inorder()
      ) %>%
      dplyr::pull(taxon) %>%
      levels()
  }

  if (ordre_taxon == "abondance sur la chronique") {
    taxons <- liste_station %>%
      dplyr::group_by(libelle_taxon) %>%
      dplyr::summarise(ab_tot = sum(resultat_taxon)) %>%
      dplyr::mutate(taxon = libelle_taxon %>%
                      forcats::fct_reorder(ab_tot)) %>%
      dplyr::pull(taxon) %>%
      levels()
  }

  if (!is.na(as.numeric(ordre_taxon))) {
    taxons <- liste_station %>%
      dplyr::filter(annee == ordre_taxon) %>%
      dplyr::group_by(libelle_taxon) %>%
      dplyr::summarise(ab_tot = sum(resultat_taxon)) %>%
      dplyr::mutate(taxon = libelle_taxon %>%
                      forcats::fct_reorder(ab_tot)) %>%
      dplyr::pull(taxon) %>%
      levels() %>%
      (function(x) {
        c(liste_station %>%
            dplyr::filter(!libelle_taxon %in% x) %>%
            dplyr::distinct(libelle_taxon) %>%
            dplyr::arrange(dplyr::desc(libelle_taxon)) %>%
            dplyr::mutate(
              taxon = libelle_taxon %>%
                forcats::fct_inorder()
            ) %>%
            dplyr::pull(taxon) %>%
            levels(),
          x
        )
      })

  }

  liste_station %>%
    dplyr::mutate(
      libelle_taxon = libelle_taxon %>%
        factor(levels = taxons)
      ) %>%
    dplyr::group_by(code_station_hydrobio, libelle_station_hydrobio,
                    annee, libelle_support, libelle_taxon) %>%
    dplyr::summarise(resultat_taxon = sum(resultat_taxon), .group = "drop") %>%
    dplyr::group_by(libelle_support) %>%
    dplyr::group_split() %>%
    purrr::map(
      function(df_temp) {
        df_temp %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              x = annee,
              y = libelle_taxon,
              size = sqrt(resultat_taxon)
            )
          ) +
          ggplot2::scale_x_continuous(
            breaks = x_breaks,
            limits = x_lims,
            sec.axis = ggplot2::sec_axis(~., breaks = x_breaks)
            ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(hjust = .95),
            legend.position = "none",
            axis.text.y = ggplot2::element_text(hjust = 0, size = 10)
          ) +
          ggplot2::labs(
            x = "", y = ""
          )
      }
    )

}

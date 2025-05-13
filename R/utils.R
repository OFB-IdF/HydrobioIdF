#' Créer une fonction de calcul de graduations entières
#'
#' @description
#' Cette fonction crée une fonction qui génère des graduations entières pour les axes
#' d'un graphique. Elle est particulièrement utile pour les graphiques où les valeurs
#' doivent être représentées par des entiers (par exemple, des comptages).
#'
#' @param n Le nombre approximatif de graduations souhaitées (par défaut : 5)
#' @param ... Arguments supplémentaires passés à la fonction pretty()
#'
#' @return Une fonction qui prend un vecteur numérique en entrée et renvoie un vecteur
#'   de graduations entières
#'
#' @details
#' La fonction retournée gère deux cas :
#' - Si toutes les valeurs sont identiques, elle renvoie cette unique valeur
#' - Sinon, elle utilise pretty() pour générer des graduations "agréables" et les arrondit
#'   à l'entier inférieur
#'
#' @examples
#' \dontrun{
#' # Utilisation avec ggplot2
#' ggplot(data, aes(x = variable)) +
#'   scale_y_continuous(breaks = integer_breaks())
#' }
#'
#' @export
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    if (length(unique(na.omit(x))) == 1) {
      breaks <- unique(na.omit(x))
    } else {
      breaks <- floor(pretty(x, n, ...))
    }

    names(breaks) <- attr(breaks, "labels")
    unique(breaks)
  }
  return(fxn)
}

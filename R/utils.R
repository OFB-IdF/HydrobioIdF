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

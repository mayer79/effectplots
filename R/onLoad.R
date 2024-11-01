.onLoad <- function(libname, pkgname) {
  op <- options()
  op.marginalplot <- list(
    marginalplot.backend = "ggplot2",
    marginalplot.fill = "lightgrey",
    marginalplot.colors = c("#CC79A7", "#009E73", "#56B4E9", "#E69F00", "#000000")
  )
  toset <- !(names(op.marginalplot) %in% names(op))
  if (any(toset)) {
    options(op.marginalplot[toset])
  }
  invisible()
}

# Darkblue: "#0072B2"

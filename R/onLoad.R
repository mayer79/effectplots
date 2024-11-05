.onLoad <- function(libname, pkgname) {
  op <- options()
  op.effectplots <- list(
    effectplots.plotly = FALSE,
    effectplots.fill = "lightgrey",
    effectplots.colors = c(
      "#CC79A7", "#009E73", "#56B4E9", "#E69F00", "#0072B2", "#000000"
    )
  )
  toset <- !(names(op.effectplots) %in% names(op))
  if (any(toset)) {
    options(op.effectplots[toset])
  }
  invisible()
}

# Color scale as per:
# scales::show_col(ggthemes::colorblind_pal()(8))

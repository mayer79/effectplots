.onLoad <- function(libname, pkgname) {
  op <- options()
  op.marginalplot <- list(
    marginalplot.backend = "ggplot2",
    marginalplot.fill = "lightgrey",
    marginalplot.line_colors = c(obs = "#E69F00", pred = "#009E73", pd = "#56B4E9")
  )
  toset <- !(names(op.marginalplot) %in% names(op))
  if (any(toset)) {
    options(op.marginalplot[toset])
  }
  invisible()
}

# Fix undefined global variable note
utils::globalVariables(
  c("bar_at", "exposure", "bar_width", "eval_at", "varying_", "value_")
)

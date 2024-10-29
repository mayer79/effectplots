# Simplified version of hist(), returns only breaks
hist2 <- function(x, breaks = "Sturges") {
  x <- x[is.finite(x)]
  if (is.character(breaks)) {
    breaks <- switch(
      tolower(breaks),
      sturges = grDevices::nclass.Sturges(x),
      `freedman-diaconis` = ,
      fd = grDevices::nclass.FD(x),
      scott = grDevices::nclass.scott(x),
      stop("unknown 'breaks' algo")
    )
  }
  if (is.function(breaks)) {
    breaks <- breaks(x)
  }
  if (length(breaks) == 1L) {
    breaks <- pretty(collapse::.range(x, na.rm = TRUE), n = breaks, min.n = 1)
  } else if (is.numeric(breaks)) {
    breaks <- sort(unique(breaks))
  } else {
    stop("Wrongly specified breaks")
  }
  return(breaks)
}

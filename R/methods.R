#' @export
print.EffectData <- function(x, ...) {
  cat(
    "'EffectData' object of length ",
    length(x),
    if (length(x) > 1) paste0(", starting with '", names(x)[1L], "'") else "",
    ": \n\n",
    sep = ""
  )
  print(x[[1L]])
  invisible(x)
}

#' @export
`[.EffectData` <- function(x, ...) {
  structure(NextMethod(), class = "EffectData")
}

#' @export
`c.EffectData` <- function(x, ...) {
  structure(NextMethod(), class = "EffectData")
}

#' @export
`+.EffectData` <- function(e1, e2) {
  c(e1, e2)
}

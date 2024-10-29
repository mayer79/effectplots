#' @export
print.marginal <- function(x, ...) {
  cat(
    "'marginal' object of length ",
    length(x),
    if (length(x) > 1) paste0(", starting with '", names(x)[1L], "'") else "",
    ": \n\n",
    sep = ""
  )
  print(x[[1L]])
  invisible(x)
}

#' @export
`[.marginal` <- function(x, ...) {
  structure(NextMethod(), class = "marginal")
}

#' @export
`c.marginal` <- function(x, ...) {
  structure(NextMethod(), class = "marginal")
}

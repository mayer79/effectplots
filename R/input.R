prep_vec <- function(x) {
  if (!is.numeric(x) && !is.logical(x)) {
    stop("Values must be numeric, or logical.")
  }
  if (anyNA(x)) {
    stop("Values can't contain NA")
  }
  if (is.double(x)) x else as.double(x)
}

prep_pred <- function(x) {
  p <- NCOL(x)
  if (is.data.frame(x)) {
    x <- x[[p]]
  } else if (p > 1L) {
    x <- x[, p]
  } else if (!is.vector(x)) {
    x <- as.vector(x)
  }
  prep_vec(x)
}

name_or_vector <- function(z, data) {
  if (length(z) == 1L && z %in% colnames(data)) {
    z <- if (is.matrix(data)) data[, z] else data[[z]]
  } else if (length(z) != nrow(data)) {
    stop("'y' or 'w' should either be NULL, a variable name, or have length nrow(data)")
  }
  return(z)
}


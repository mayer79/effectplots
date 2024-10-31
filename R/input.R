prep_pred <- function(x) {
  p <- NCOL(x)
  if (is.data.frame(x)) {
    x <- x[[p]]
  } else if (p > 1L) {
    x <- x[, p]
  } else if (!is.vector(x)) {
    x <- as.vector(x)
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop("Predictions must be numeric or logical.")
  }
  if (anyNA(x)) {
    stop("Predictions can't contain NA")
  }
  if (is.double(x)) x else as.double(x)
}

basic_check <- function(z, n, nms) {
  is.null(z) || (length(z) == 1L && z %in% nms) || (is.vector(z) && length(z) == n)
}


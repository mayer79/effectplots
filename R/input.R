#' Prepare Predictions
#'
#' Internal function used to prepare the result of `pred_fun()`. It
#' - picks the `which_pred` column if there are multiple columns,
#' - it turns the result to a double vector,
#' - it stops if there are missing values,
#' - it stops if the values are not numeric or logical.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A vector or matrix-like of predictions.
#' @inheritParams feature_effects
#' @returns A numeric vector.
prep_pred <- function(x, trafo = NULL, which_pred = NULL) {
  p <- NCOL(x)
  if (is.null(which_pred)) {
    which_pred <- p
  }
  if (is.data.frame(x)) {
    x <- x[[which_pred]]
  } else if (p > 1L) {
    x <- x[, which_pred]
  }
  if (!is.vector(x)) {
    x <- as.vector(x)
  }
  if (!is.null(trafo)) {
    x <- trafo(x)
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop("Predictions must be numeric or logical.")
  }
  if (anyNA(x)) {
    stop("Predictions can't contain NA")
  }
  return(as.double(unname(x)))
}

#' Input Checks
#'
#' Internal function used to check if the input is consistent, i.e., it should either
#' be `NULL`, have length 1 and be in `nms`, or be a vector of length n.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A vector.
#' @param n Size of reference data.
#' @param nms Column names of reference data.
#' @returns A logical vector of length 1.
basic_check <- function(z, n, nms) {
  is.null(z) || (length(z) == 1L && z %in% nms) || (is.vector(z) && length(z) == n)
}

#' Consistent Subsampling
#'
#' Internal function used to subsample a data-like and corresponding weights, given that
#' they are too long.
#'
#' @noRd
#' @keywords internal
#'
#' @param data A matrix-like.
#' @param nmax If data is larger than `nmax`, we will subsample.
#' @param w Optional vector with case weights.
#' @returns A list with subsampled `data`, `w`, and the indices used to subsample data.
#'   (The latter only if subsampling was done).
.subsample <- function(data, nmax, w = NULL) {
  N <- nrow(data)
  if (N <= nmax) {
    return(list(X = data, w = w))
  }
  ix <- sample.int(N, nmax)
  return(list(X = collapse::ss(data, ix), w = if (!is.null(w)) w[ix], ix = ix))
}

#' Check Feature Type
#'
#' Internal function used to check if a feature is of acceptable type.
#'
#' @noRd
#' @keywords internal
#'
#' @param z A matrix or vector/factor.
#' @returns A length-one logical.
check_v_type <- function(z) {
  is.numeric(z) || is.logical(z) || is.factor(z) || is.character(z)
}

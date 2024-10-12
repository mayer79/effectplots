# Copied from hstats:::rep_rows
rep_rows <- function (x, i) {
  if (!(all(class(x) == "data.frame"))) {
    return(x[i, , drop = FALSE])
  }
  out <- lapply(x, function(z) if (length(dim(z)) != 2L)
    z[i]
    else z[i, , drop = FALSE]
  )
  attr(out, "row.names") <- .set_row_names(length(i))
  class(out) <- "data.frame"
  out
}

# Simplified from hstats:::wrowmean_vector()
wrowmean <- function(x, ngroups = 1L, w = NULL) {
  dim(x) <- c(length(x) %/% ngroups, ngroups)
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w)
}

# Simplified from hstats:::ice_raw()
ice_raw <- function (object, v, X, grid, pred_fun = stats::predict, ...) {
  X_pred <- rep_rows(X, rep.int(seq_len(nrow(X)), length(grid)))
  grid_pred <- rep(grid, each = nrow(X))

  if (is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred
  } else {
    X_pred[, v] <- grid_pred
  }
  prepare_pred(pred_fun(object, X_pred, ...))
}

# Simplified from hstats:::gwColMeans()
gwColMeans <- function(x, g, w = NULL) {
  if (is.null(w)) {
    w <- rep.int(1, NROW(x))
  } else {
    x <- x * w
  }
  denom <- as.numeric(rowsum(w, group = g))
  list(M = rowsum(x, group = g) / denom, w = denom)
}

prepare_pred <- function(x) {
  if (NCOL(x) > 1L) {
    stop("Only univariate predictions are handled.")
  }
  if (is.data.frame(x)) {
    x <- x[[1L]]
  }
  if (!is.vector(x)) {
    x <- as.vector(x)
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop("Predictions must be numeric or logical.")
  }
  return(as.double(x))
}

prepare_yw <- function(z, X) {
  if (length(z) == 1L && z %in% colnames(X)) {
    z <- if (is.matrix(X)) X[, z] else X[[z]]
  } else if (length(z) != nrow(X)) {
    stop("'y' or 'w' should either be NULL, a variable name, or have length nrow(X)")
  }
  return(prepare_pred(z))
}





#' Histogram Bin Construction
#'
#' Creates histogram of vector/factor `x`. In the discrete case, no binning is done.
#' Otherwise, the values are optionally trimmed and then passed to [hist()]. Compared
#' with [hist()], the function also returns the binned values of `x`.
#'
#' @param x A vector or factor to be binned.
#' @inheritParams hist
#' @inheritParams univariate_grid
#' @returns A list with binned "x", vector of "breaks", bin midpoints "grid", and a
#'   logical flag "discrete" indicating whether the values have not been binned.
#' @seealso See [marginal()] for examples.
hist2 <- function(x, breaks = 17L, trim = c(0.01, 0.99), right = TRUE, na.rm = TRUE) {
  g <- unique(x)
  if (!is.numeric(x) || (length(breaks) == 1L && is.numeric(breaks) && length(g) <= breaks)) {
    g <- sort(g, na.last = if (na.rm) NA else TRUE)
    return(list(x = x, breaks = g, grid = g, discrete = TRUE))
  }

  # Trim outliers before histogram construction?
  if (trim[1L] == 0 && trim[2L] == 1) {
    xx <- x
  } else {
    r <- stats::quantile(x, probs = trim, names = FALSE, type = 1L, na.rm = TRUE)
    xx <- x[x >= r[1L] & x <= r[2L]]
  }
  h <- hist(
    xx, breaks = breaks, include.lowest = TRUE, right = right, plot = FALSE
  )
  b <- h$breaks
  ix <- findInterval(
    x, vec = b, left.open = right, rightmost.closed = TRUE, all.inside = TRUE
  )
  g <- h$mids
  if (!na.rm && anyNA(x)) {
    g <- c(g, NA)
  }
  list(x = g[ix], breaks = b, grid = g, discrete = FALSE)
}


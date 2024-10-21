wins_prob <- function(x, probs = 0:1, nmax = 1e5) {
  if (!is.numeric(x) || (probs[1L] == 0 && probs[2L] == 1)) {
    return(x)
  }
  xs <- if (length(x) > nmax) sample(x, nmax) else x
  # If one of the probs is 0 or 1, the following line is inefficient
  q <- stats::quantile(xs, probs = probs, na.rm = TRUE, names = FALSE, type = 1L)
  winsorize(x, low = q[1L], high = q[2L])
}

# pmin() and pmax() are surprisingly fast for large vectors
# and need less memory than subsetting
winsorize <- function(x, low = -Inf, high = Inf) {
  if (low != -Inf) {
    x <- pmax(x, low)
  }
  if (high != Inf) pmin(x, high) else x
}

#' Stack some Columns (from hstats)
#'
#' Internal function used in the plot method for "pd" objects. The function brings
#' wide columns `to_stack` (the prediction dimensions) into long form.
#'
#' @noRd
#' @keywords internal
#'
#' @param data A data.frame.
#' @param to_stack Column names in `data` to bring from wide to long form.
#' @returns
#'   A data.frame with variables not in `to_stack`, a column "varying_" with
#'   the column name from `to_stack`, and finally a column "value_" with stacked values.
poor_man_stack <- function(data, to_stack) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  keep <- setdiff(colnames(data), to_stack)
  out <- lapply(
    to_stack,
    FUN = function(z) cbind.data.frame(data[keep], varying_ = z, value_ = data[, z])
  )
  out <- do.call(rbind, out)
  transform(out, varying_ = factor(varying_, levels = to_stack))
}

grouped_mean <- function(x, g, w = NULL) {
  if (is.null(w)) {
    w <- rep.int(1, length(g))
  } else if (!is.null(x)) {
    x <- x * w
  }
  suppressWarnings(exposure <- rowsum(w, group = g))  # silence warning about missings
  colnames(exposure) <- "exposure"
  if (is.null(x)) {
    return(exposure)
  }
  suppressWarnings(S <- rowsum(x, group = g))         # silence warning about missings
  cbind(S / as.numeric(exposure), exposure)
}

wrowmean <- function(x, ngroups = 1L, w = NULL) {
  dim(x) <- c(length(x) %/% ngroups, ngroups)
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w)
}

# Copied from hstats:::rep_rows
rep_rows <- function(x, i) {
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

partial_dep <- function(object, v, X, grid, pred_fun = stats::predict, w = NULL, ...) {
  X_pred <- rep_rows(X, rep.int(seq_len(nrow(X)), length(grid)))
  grid_pred <- rep(grid, each = nrow(X))
  if (is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred
  } else {
    X_pred[, v] <- grid_pred
  }
  preds <- prep_vector(pred_fun(object, X_pred, ...))
  return(wrowmean(preds, ngroups = length(grid), w = w))
}

prep_vector <- function(x) {
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
  if (is.double(x)) x else as.double(x)
}

name_or_vector <- function(z, data) {
  if (length(z) == 1L && z %in% colnames(data)) {
    z <- if (is.matrix(data)) data[, z] else data[[z]]
  } else if (length(z) != nrow(data)) {
    stop("'y' or 'w' should either be NULL, a variable name, or have length nrow(data)")
  }
  return(z)
}


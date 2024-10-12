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

# Adapted from hstats:::wrowmean_vector()
wrowmean <- function(x, ngroups = 1L, w = NULL) {
  dim(x) <- c(length(x) %/% ngroups, ngroups)
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w)
}

# Adapted from hstats:::gwColMeans()
gwColMeans <- function(x, g, w = NULL) {
  if (is.null(w)) {
    w <- rep.int(1, NROW(x))
  } else {
    x <- x * w
  }
  denom <- as.numeric(rowsum(w, group = g))
  list(M = rowsum(x, group = g) / denom, w = denom)
}


# Adapted from {hstats}
partial_dep <- function(
    object, v, X, grid, pred_fun = stats::predict, pd_n = 500, w = NULL, ...
) {
  # Reduce data
  if (nrow(X) > pd_n) {
    ix <- sample(nrow(X), pd_n)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }

  # ICE
  X_pred <- rep_rows(X, rep.int(seq_len(nrow(X)), length(grid)))
  grid_pred <- rep(grid, each = nrow(X))
  if (is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred
  } else {
    X_pred[, v] <- grid_pred
  }
  preds <- prep_vector(pred_fun(object, X_pred, ...))

  # PD
  wrowmean(preds, ngroups = length(grid), w = w)
}

prep_vector <- function(x) {
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

calculate_stats <- function(
    x,
    pred = NULL,
    y = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete = NULL
) {
  if (is.null(pred) && is.null(y)) {
    stop("Either pred or y must be present")
  }
  g <- unique(x)

  # DISCRETE
  if (!is.numeric(x) || isTRUE(discrete) || length(g) <= 2L) {
    g <- sort(g, na.last = TRUE)  # Same order as gwCOlMeans() resp. rowsum(sorted)
    if (is.numeric(g)) {
      g <- factor(g)  # To ensure nice plot scales
    }
    S <- gwColMeans(cbind(pred = pred, obs = y), g = x, w = w)
    out <- data.frame(bar_center = g, bar_width = 0.7, exposure = S$w, eval_at = g, S$M)
    rownames(out) <- NULL
    discrete <- TRUE
  } else {
    # "CONTINUOUS"
    H <- graphics::hist(x, breaks = breaks, right = right, plot = FALSE)
    g <- H$mids
    if (anyNA(x)) {
      g <- c(g, NA)
    }
    # Integer encoding
    ix <- findInterval(
      x, vec = H$breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
    )
    S <- gwColMeans(cbind(eval_at = x, pred = pred, obs = y), g = ix, w = w)
    S <- cbind(exposure = S$w, S$M)
    out <- data.frame(bar_center = g, bar_width = diff(H$breaks))
    out[rownames(S), colnames(S)] <- S
    s <- is.na(out$exposure)
    if (any(s)) {
      out[s, "exposure"] <- 0
      out[s, "eval_at"] <- out[s, "bar_center"]
    }
    discrete <- FALSE
  }

  return(list(data = out, discrete = discrete))
}


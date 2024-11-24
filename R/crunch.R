#' Turn Input either Double or Factor
#'
#' @noRd
#' @keywords internal
#'
#' @param x A vector or factor.
#' @param m If `x` is numeric: Up to which number of disjoint values is it a factor?
#' @param ix_sub Subset for pre-check. If not `NULL`, length(x) > 10000.
#' @returns
#'   A double vector if x is numeric with > m disjoint values.
#'   Otherwise, a factor with explicit missings.
factor_or_double <- function(x, m = 5L, ix_sub = NULL) {
  if (!is.numeric(x)) {
    return(collapse::qF(x, sort = is.factor(x), na.exclude = FALSE, drop = TRUE))
  }
  if (is.double(x)) {
    # {collapse} seems to distinguish positive and negative zeros
    # https://github.com/SebKrantz/collapse/issues/648
    # Adding 0 to a double turns negative 0 to positive ones (ISO/IEC 60559)
    collapse::setop(x, "+", 0.0)
  }
  if (!is.null(ix_sub)) {  # we have >10k values
    if (m >= length(ix_sub)) {
      stop("Too large value for m")
    }
    if (collapse::fnunique(x[ix_sub]) > m) {
      return(as.double(x))
    }
  }
  xf <- collapse::qF(x, sort = FALSE, na.exclude = FALSE)
  if (collapse::fnlevels(xf) > m) as.double(x) else xf
}


#' IQR-based Outlier Capping
#'
#' Internal function used to cap/clip/winsorize outliers via the boxplot rule.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector.
#' @param m How many IQRs from the quartiles do we start capping?
#' @param ix_sub Subset used to calculate approximate quartiles. Can be `NULL`.
#' @returns Lower and upper bound, or `NULL`.
wins_iqr <- function(x, m = 1.5, ix_sub = NULL) {
  if (!is.null(ix_sub)) {
    x <- x[ix_sub]
  }
  q <- collapse::fquantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  r <- m * diff(q)
  if (r <= 0) {
    return(NULL)
  }
  return(q + c(-r, r))
}

#' Break Calculation like hist()
#'
#' Internal function used to calculate breaks.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector.
#' @param breaks An integer, a vector, or "Sturges".
#' @param outlier_iqr See [feature_effects()].
#' @param ix_sub An optional subsetting vector to calculate quartiles.
#' @returns A vector of pretty breaks.
hist2 <- function(x, breaks = "Sturges", outlier_iqr = 0, ix_sub = NULL) {
  if (is.character(breaks)) {
    if (tolower(breaks) == "sturges") {
      breaks <- ceiling(log2(length(x)) + 1)
    } else {
      stop("unknown 'breaks' algo")
    }
  }
  if (length(breaks) == 1L) {
    r <- collapse::.range(x, na.rm = TRUE)
    if (outlier_iqr > 0 && is.finite(outlier_iqr)) {
      r2 <- wins_iqr(x, m = outlier_iqr, ix_sub = ix_sub)
      r[1L] <- max(r[1L], r2[1L])
      r[2L] <- min(r[2L], r2[2L])
    }
    breaks <- pretty(r, n = breaks, min.n = 1)
  } else if (is.numeric(breaks)) {
    breaks <- sort(unique(breaks))
  } else {
    stop("Wrongly specified breaks")
  }
  return(breaks)
}

#' Fast Grouped Statistics
#'
#' Internal function used to calculate grouped counts, exposures, means and standard
#' deviations of a matrix `x`. Might be replaced by collapse::qsu().
#'
#' @noRd
#' @keywords internal
#'
#' @param x A data.frame or matrix.
#' @param g A factor.
#' @param w Optional vector with case weights.
#' @returns A matrix with counts, weights, means and standard deviations of
#'   columns in `x`.
grouped_stats <- function(x, g, w = NULL, sd_cols = colnames(x)) {
  N <- collapse::fnobs(g, g = g, use.g.names = TRUE)
  if (!is.null(w)) {
    weight <- collapse::fsum(w, g = g, fill = TRUE, use.g.names = FALSE)
  } else {
    weight <- N
  }
  out <- cbind(N = N, weight = weight)
  if (is.null(x)) {
    return(out)
  }
  M <- as.matrix(collapse::fmean(x, g = g, w = w, use.g.names = FALSE))
  colnames(M) <- paste0(colnames(M), "_mean")

  # Add some standard deviations
  if (length(sd_cols)) {
    S <- as.matrix(
      collapse::fsd(collapse::ss(x, , sd_cols), g = g, w = w, use.g.names = FALSE)
    )
    colnames(S) <- paste0(colnames(S), "_sd")
    return(cbind(out, M, S))
  }
  cbind(out, M)
}

#' Fast findInterval()
#'
#' Internal function used to bin a numeric `x`. Uses `findInterval()` when breaks are
#' unequally long, and an algorithm adapted from `spatstat.utils::fastFindInterval()`
#' otherwise.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector to bin.
#' @param br A monotonically increasing vector of breaks.
#' @param right Right closed bins (`TRUE`, default) or not?
#' @returns Binned version of `x`.
# Fast implementation of
findInterval2 <- function(x, breaks, right = TRUE) {
  nbreaks <- length(breaks)
  if (nbreaks <= 2L) {
    return(rep.int(1L, times = length(x)))
  }
  # from hist2.default()
  h <- diff(breaks)
  if (!all(h > 0)) {
    stop("Breaks must be strictly increasing")
  }
  if (diff(range(h)) < 1e-07 * mean(h)) {  # equidist
    findInterval_equi(
      as.double(x),
      low = as.double(breaks[1L]),
      high = as.double(breaks[nbreaks]),
      nbin = nbreaks - 1L,
      right = as.logical(right)
    )
  } else {
    findInterval(
      x, vec = breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
    )
  }
}

#' Check if Vector is Numeric
#'
#' Internal function used to check if a variable is numeric *and* has more than
#' m disjoint values. The function shines for extremely long vectors (~1e7).
#'
#' @noRd
#' @keywords internal
#'
#' @param x A vector or factor.
#' @param m How many disjoint values will return FALSE?
#' @returns `TRUE` if x is numeric with > m disjoint values, and `FALSE` otherwise.
is_continuous <- function(x, m = 5L) {
  M <- 10000L
  if (!is.numeric(x)) {
    return(FALSE)
  }
  if (m >= M) {
    stop("Too large value for m")
  }
  if (length(x) <= M) {
    return(collapse::fnunique(x) > m)
  }
  if (collapse::fnunique(x[1L:M]) > m) {
    return(TRUE)
  }
  return(collapse::fnunique(x) > m)
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
#' @param nmax Maximal number of observations used to calculate capping limits.
#' @returns Like `x`, but eventually capped.
wins_iqr <- function(x, m = 1.5, nmax = 10000L) {
  xs <- if (length(x) > nmax) sample(x, nmax) else x
  q <- collapse::fquantile(xs, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  r <- m * diff(q)
  if (r <= 0) {
    return(x)
  }
  winsorize(x, low = q[1L] - r, high = q[2L] + r)
}

#' Cap a Vector
#'
#' Internal function used to cap/clip/winsorize a numeric vector at low/high limits.
#' Note: For large vectors, pmin() and pmax() are surprisingly fast.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector to be capped.
#' @param low Lower capping limit.
#' @param high Upper capping limit.
#' @returns A capped vector.
winsorize <- function(x, low = -Inf, high = Inf) {
  r <- collapse::.range(x, na.rm = TRUE)
  if (r[1L] < low) {
    x <- pmax(x, low)
  }
  if (r[2L] > high) pmin(x, high) else x
}

#' Break Calculation like hist()
#'
#' Internal function used to calculate breaks with the same `breaks` as `hist()`.
#' The only difference is that when `breaks` is a string and length(x) >= 100k, the
#' function returns 50.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector.
#' @param breaks An integer, a vector, a string, or a function.
#' @returns A vector of pretty breaks.
hist2 <- function(x, breaks = "Sturges") {
  x <- collapse::na_rm(x)
  if (is.character(breaks)) {
    breaks <- tolower(breaks)
    if (breaks == "sturges") {
      breaks <- grDevices::nclass.Sturges(x)
    } else if (length(x) >= 1e5) {
      breaks <- 50L
    } else if (breaks == "scott") {
      breaks <- min(50L, grDevices::nclass.scott(x))
    } else if (breaks %in% c("fd", "freedman-diaconis")) {
      breaks <- min(50L, grDevices::nclass.FD(x))
    } else {
      stop("unknown 'breaks' algo")
    }
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
  N <- collapse::fsum(rep.int(1L, times = length(g)), g = g, fill = TRUE)
  if (!is.null(w)) {
    weight <- collapse::fsum(w, g = g, fill = TRUE)
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

# Pure R implementation of spatstat.utils::fastFindInterval()
findInterval2 <- function(x, breaks, right = TRUE) {
  m <- length(breaks)
  if (m <= 2L) {
    return(rep.int(1L, times = length(x)))
  }
  # from hist2.default()
  h <- diff(breaks)
  equidist <- diff(range(h)) < 1e-07 * mean(h)
  if (!equidist) {
    return(
      findInterval(
        x, vec = breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
      )
    )
  }
  D <- (breaks[m] - breaks[1L]) / (m - 1)
  x <- (x - breaks[1L]) / D
  x <- if (right) as.integer(ceiling(x)) else as.integer(x) + 1L
  x[x <= 1L] <- 1L
  x[x >= m - 1L] <- m - 1L
  return(x)
}

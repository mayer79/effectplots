#' Checks if many distinct
#'
#' Internal function used to check if a numeric variable has more than
#' m disjoint values. The function shines for extremely long vectors (~1e7).
#'
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector.
#' @param m How many disjoint values will return FALSE?
#' @returns `TRUE` (if x has more than m levels), and `FALSE` otherwise.
more_than_m_distinct <- function(x, m = 5L) {
  if (length(x) <= 10000L) {
    return(collapse::fnunique(x) > m)
  }
  if (collapse::fnunique(x[1L:10000L]) > m) {
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
#' @param x A matrix.
#' @param g A grouping vector/factor.
#' @param w Optional vector with case weights.
#' @returns A data.frame with Counts, weight sums, means and standard deviations of
#'   each column in `x`.
grouped_stats <- function(x, g, w = NULL) {
  # returns rows in order sort(unique(x)) + NA, or levels(x) + NA (if factor)
  if (!is.factor(g)) {
    g <- collapse::qF(g, sort = TRUE)
  }
  N <- collapse::fsum.default(rep.int(1L, times = length(g)), g = g)
  if (!is.null(w)) {
    weight <- collapse::fsum.default(w, g = g)
  } else {
    weight <- N
  }
  out <- cbind(N = N, weight = weight)
  if (is.null(x)) {
    return(out)
  }
  M <- collapse::fmean.matrix(x, g = g, w = w, use.g.names = FALSE)
  S <- collapse::fsd.matrix(x, g = g, w = w, use.g.names = FALSE)
  colnames(M) <- paste0(colnames(S), "_mean")
  colnames(S) <- paste0(colnames(S), "_sd")
  return(cbind(out, M, S))
}

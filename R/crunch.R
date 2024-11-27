#' Turn Input either Double or Factor
#'
#' @noRd
#' @keywords internal
#'
#' @param x A vector or factor.
#' @param m If `x` is numeric: Up to which number of disjoint values is it a factor?
#' @param ix_sub Subset for pre-check. If not `NULL`, length(x) > 9997.
#' @returns
#'   A double vector if x is numeric with > m disjoint values.
#'   Otherwise, a factor with explicit missings.
factor_or_double <- function(x, m = 5L, ix_sub = NULL) {
  if (!is.numeric(x)) {
    if (is.factor(x)) {
      return(collapse::qF(x, sort = TRUE, na.exclude = !anyNA(unclass(x)), drop = TRUE))
    }
    return(collapse::qF(x, sort = FALSE, na.exclude = FALSE))
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
  if (r <= 0 || is.na(r)) {
    return(NULL)
  }
  return(q + c(-r, r))
}

#' Fast Break Calculation
#'
#' Internal function used to calculate breaks.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector.
#' @param breaks An integer, a vector, or "Sturges" (the default).
#' @param outlier_iqr See [feature_effects()].
#' @param ix_sub An optional subsetting vector to calculate quartiles.
#' @returns A vector of pretty breaks.
fbreaks <- function(x, breaks = "Sturges", outlier_iqr = 0, ix_sub = NULL) {
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

#' Fast cut()
#'
#' Bins a numeric vector `x` into bins specified by `breaks`.
#' Values outside the range of `breaks` will be placed in the lowest or highest bin.
#' Set `labels = FALSE` to return integer codes only, and `explicit_na = TRUE` for
#' maximal synergy with the "collapse" package.
#' Uses the logic of `spatstat.utils::fastFindInterval()` for equi-length bins.
#'
#' @param x A numeric vector.
#' @param breaks A monotonically increasing numeric vector of breaks.
#' @param labels A character vector of length `length(breaks) - 1` with bin labels.
#'   By default (`NULL`), the levels `c("1", "2", ...)` are used. Set to `FALSE`
#'   to return raw integer codes.
#' @param right Right closed bins (`TRUE`, default) or not?
#' @param explicit_na If `TRUE`, missing values are encoded by the bin value
#'   `length(breaks)`, having `NA` as corresponding factor level. The factor will get
#'   the additional class "na.included".
#' @returns Binned version of `x`. Either a factor or integer codes.
#' @export
#' @examples
#' x <- c(NA, 1:10)
#' fcut(x, breaks = c(3, 5, 7))
#' fcut(x, breaks = c(3, 5, 7), right = FALSE)
#' fcut(x, breaks = c(3, 5, 7), labels = FALSE)
#'
fcut <- function(x, breaks, labels = NULL, right = TRUE, explicit_na = FALSE) {
  nb <- length(breaks) - 1L
  stopifnot(
    is.numeric(x),
    nb >= 1L,
    !is.unsorted(breaks),
    is.null(labels) || isFALSE(labels) || (is.character(labels) && length(labels) == nb)
  )
  codes_only <- isFALSE(labels)
  if (!is.character(labels)) {
    labels <- as.character(seq_len(nb))  # need for equi-length case even if codes_only
  }

  # From hist2.default()
  h <- diff(breaks)
  if (!all(h > 0)) {
    stop("Breaks must be strictly increasing")
  }

  # Three cases: one bin, equi-length bins, other
  if (nb == 1L) {
    out <- rep.int(1L, length(x))
    if (anyNA(x)) {
      if (explicit_na) {
        out[is.na(x)] <- 2L
        labels <- c(labels, NA_character_)
      } else {
        out[is.na(x)] <- NA_integer_
      }
    }
  } else if (diff(range(h)) < 1e-07 * mean(h)) {  # spatstat.utils::fastFindInterval()
    return(
      findInterval_equi(
        as.double(x),
        low = as.double(breaks[1L]),
        high = as.double(breaks[nb + 1L]),
        nbin = nb,
        right = as.logical(right),
        labels = labels,
        explicit_na = as.logical(explicit_na),
        codes_only = as.logical(codes_only)
      )
    )
  } else {
    out <- findInterval(
      x, vec = breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
    )
    if (explicit_na && anyNA(x)) {
      out[is.na(x)] <- nb + 1L
      labels <- c(labels, NA_character_)
    }
  }
  if (codes_only) {
    return(out)
  }
  structure(out, levels = labels, class = c("factor", if (explicit_na) "na.included"))
}


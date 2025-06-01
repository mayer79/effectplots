#' Find rare factor levels (currently unused)
#'
#' Internal function to find rare factor levels. NA levels are never
#' considered as being rare. Only used by `flump()`.
#'
#' @noRd
#' @keywords internal
#'
#' @param f A factor.
#' @param m How many rare levels to find?
#' @param w Optional case weights.
#' @returns A character vector with m rare levels, or `NULL`.
rare_levels <- function(f, m, w = NULL) {
  if (m < 2L) {
    return(NULL)
  }
  if (is.null(w)) {
    N <- collapse::fnobs(f, g = f)
  } else {
    N <- collapse::fsum(w, g = f, fill = TRUE)
  }
  N <- sort(N[!is.na(names(N))]) # Never consider NA
  m <- min(m, length(N))
  return(names(N)[seq_len(m)])
}

#' Collapse factor levels (currently unused)
#'
#' Internal function to collapse factor levels. Only used in `flump()`.
#' @noRd
#' @keywords internal
#'
#' @param f A factor produces with `collapse::qF(, na.exclude=False)`.
#' @param to_combine Levels to combine.
#' @param other_level Name of the new level, e.g., "Other 3" if three levels are combined.
#' @returns A factor with combined levels.
combine_levels <- function(f, to_combine, other_level = "Other") {
  if (length(to_combine) < 2L) {
    return(f)
  }
  lvl <- levels(f)
  to_keep <- setdiff(lvl, to_combine)
  if (other_level %in% to_keep) {
    stop("'other_level' already exists")
  }
  new_levels <- c(to_keep, other_level)
  lvl[!(lvl %in% to_keep)] <- other_level

  # Like in forcats:::lvls_revalue()
  # Note that (implicit) NA values in f remain NA in out
  map_to_new_codes <- match(lvl, new_levels)
  out <- map_to_new_codes[f]
  attributes(out) <- attributes(f)
  attr(out, "levels") <- new_levels
  return(out)
}

#' Lump rare factor levels (currently unused)
#'
#' Internal function to lump rare factor levels into a new level. Note that missing
#' values (even if explicitly encoded as level NA) are not combined.
#'
#' @noRd
#' @keywords internal
#'
#' @param f A factor.
#' @param combine_m How many rare levels to combine?
#' @param w Optional case weights.
#' @returns
#'   A list with three elements: "f" is a factor with combined levels,
#'   "combined" is a character vector with the combined levels, and "other_level"
#'   is the name of the new level.
flump <- function(f, combine_m, w = NULL) {
  stopifnot(
    is.factor(f),
    combine_m > 1L,
    is.null(w) || length(f) == length(w)
  )
  to_combine <- rare_levels(f, m = combine_m, w = w)
  if (length(to_combine) <= 1L) {
    return(f = f, combined = NULL, other_level = NULL)
  }
  other_level <- paste("Other", length(to_combine))

  out <- list(
    f = combine_levels(f, to_combine = to_combine, other_level = other_level),
    combined = to_combine,
    other_level = other_level
  )
  return(out)
}

#' Prepares discrete feature for grouped operations of {collapse}
#'
#' @description
#' This function returns two elements:
#' 1. A factor `g` in the same order as `x` used for grouped calculations and
#' 2. a vector/factor `bin_mid` of unique values of `x` used for partial dependence.
#'
#' Comments on `g`:
#' - The order of values in `g` correspond to the order in `x`.
#' - Missing values in `x` are represented as an explicit NA level in `g`. This helps
#'   to avoid unnecessary copies of `g` in subsequent grouped calculations.
#' - There are not empty levels.
#'
#' Comments on `bin_mid`:
#' - The order of values in `bin_mid` is the same as the levels of `g`.
#' - It will retain the original class.
#' - If `x` is a factor, so will be `bin_mid`. It will keep the original levels.
#' @noRd
#' @keywords internal
#'
#' @param x A discrete vector or factor.
#' @returns A list with two elements: "g" is a factor representing `x`, while the
#'  second element "bin_mid" represent corresponding evaluation points for partial
#'  dependence.
qF2 <- function(x) {
  if (is.factor(x)) {
    # Safe way to keep attributes for PDP while ensuring correct order
    bin_mid <- sort(collapse::funique(x), na.last = TRUE)
    g <- collapse::qF(
      x,
      ordered = is.ordered(x),
      sort = TRUE,
      na.exclude = !anyNA(unclass(x)),
      drop = TRUE
    )
    return(list(g = g, bin_mid = bin_mid))
  }
  g <- collapse::qG(x, sort = FALSE, na.exclude = FALSE, return.groups = TRUE)
  # The following lines could be a solution to keep attributes other than class
  # bin_mid <- attr(g, "groups")
  # attributes(bin_mid) <- attributes(x)
  return(list(g = collapse::as_factor_qG(g), bin_mid = attr(g, "groups")))
}

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
#'   Otherwise, a list with two elements. The first "g" is a factor representing
#'   `x`, while the second element "bin_mid" represent corresponding evaluation
#'   points for partial dependence.
factor_or_double <- function(x, m = 5L, ix_sub = NULL) {
  if (!is.numeric(x)) {
    return(qF2(x))
  }
  if (!is.null(ix_sub)) { # we have >10k values
    if (m >= length(ix_sub)) {
      stop("Too large value for m")
    }
    if (collapse::fnunique(x[ix_sub]) > m) {
      return(as.double(x))
    }
  }
  xf <- qF2(x)
  if (length(xf$bin_mid) > m) as.double(x) else xf
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
      if (!is.null(r2)) {
        r[1L] <- max(r[1L], r2[1L])
        r[2L] <- min(r[2L], r2[2L])
      }
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
#' @returns Binned version of `x`. Either a factor, or integer codes.
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
    labels <- as.character(seq_len(nb)) # need for equi-length case even if codes_only
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
  } else if (diff(range(h)) < 1e-07 * mean(h)) { # spatstat.utils::fastFindInterval()
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
      x,
      vec = breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
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

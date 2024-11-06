wins_iqr <- function(x, m = 1.5, nmax = 10000L) {
  xs <- if (length(x) > nmax) sample(x, nmax) else x
  q <- collapse::fquantile(xs, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  r <- m * diff(q)
  if (r <= 0) {
    return(x)
  }
  winsorize(x, low = q[1L] - r, high = q[2L] + r)
}

# pmin() and pmax() are surprisingly fast for large vectors
# and need less memory than subsetting
winsorize <- function(x, low = -Inf, high = Inf) {
  r <- collapse::.range(x, na.rm = TRUE)
  if (r[1L] < low) {
    x <- pmax(x, low)
  }
  if (r[2L] > high) pmin(x, high) else x
}

# Simplified version of hist(), returns only breaks
hist2 <- function(x, breaks = "Sturges") {
  x <- x[is.finite(x)]
  if (is.character(breaks)) {
    breaks <- switch(
      tolower(breaks),
      sturges = grDevices::nclass.Sturges(x),
      `freedman-diaconis` = ,
      fd = grDevices::nclass.FD(x),
      scott = grDevices::nclass.scott(x),
      stop("unknown 'breaks' algo")
    )
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

# Later replace by collapse::qsu()
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

calculate_stats <- function(
    v,
    PYR,
    w,
    data,
    breaks,
    right,
    discrete_m,
    outlier_iqr,
    object,
    pred_fun,
    trafo,
    which_pred,
    pd_data,
    ale_data,
    ale_bin_size,
    ...
) {
  x <- if (is.matrix(data)) data[, v] else data[[v]]
  if (is.double(x)) {
    # {collapse} seems to distinguish positive and negative zeros
    # https://github.com/SebKrantz/collapse/issues/648
    # Adding 0 to a double turns negative 0 to positive ones (ISO/IEC 60559)
    collapse::setop(x, "+", 0.0)
  }
  g <- collapse::funique(x)
  num <- is.numeric(x) && (length(g) > discrete_m)

  if (is.null(PYR) && is.null(pd_data) && (!num || is.null(ale_data))) {
    return(NULL)
  }

  # DISCRETE
  if (!num) {
    # Ordered by sort(g) (+ NA). For factors: levels(x) (+ NA)
    M <- grouped_stats(PYR, g = x, w = w)
    g <- sort(g, na.last = TRUE)
    out <- data.frame(bin_mid = g, bin_width = 0.7, bin_mean = g, M)
    rownames(out) <- NULL
  } else {
    # "CONTINUOUS" case. Tricky because there can be empty bins.
    if (outlier_iqr > 0 && is.finite(outlier_iqr)) {
      x <- wins_iqr(x, m = outlier_iqr)
    }
    br <- hist2(x, breaks = breaks)
    g <- 0.5 * (br[-1L] + br[-length(br)])  # mids
    gix <- seq_along(g)
    bin_width <- diff(br)
    if (anyNA(x)) {
      g <- c(g, NA)
      gix <- c(gix, NA)
      bin_width <- c(bin_width, NA)  #  Can't be plotted anyway
    }
    out <- data.frame(
      bin_mid = g, bin_width = bin_width, bin_mean = g, N = 0, weight = 0
    )

    # Integer encoding
    ix <- collapse::qF(
      findInterval(
        x, vec = br, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
      ),
      sort = TRUE
    )
    M <- cbind(
      bin_mean = collapse::fmean.default(x, g = ix, w = w),
      grouped_stats(PYR, g = ix, w = w)
    )
    reindex <- match(as.integer(rownames(M)), gix)
    out[reindex, colnames(M)] <- M  # Fill gaps
  }

  # Add partial dependence
  if (!is.null(pd_data)) {
    out$pd <- .pd(
      object = object,
      v = v,
      data = pd_data$X,
      grid = out$bin_mean,
      pred_fun = pred_fun,
      trafo = trafo,
      which_pred = which_pred,
      w = pd_data$w,
      ...
    )
  }

  # Add ALE
  if (!is.null(ale_data)) {
    out$ale <- NA
    if (num) {
      ale <- .ale(
        object = object,
        v = v,
        data = ale_data$X,
        breaks = br,
        right = right,  # does not matter because we pass g
        pred_fun = pred_fun,
        trafo = trafo,
        which_pred = which_pred,
        bin_size = ale_bin_size,
        w = ale_data$w,
        g = ix[ale_data$ix],
        ...
      )
      ok <- !is.na(out$bin_mid)

      # Centering possible?
      cvars <- intersect(c("pd", "pred_mean", "y_mean"), colnames(out))
      if (length(cvars)) {
        w_ok <- out$weight[ok]
        ale <- ale + collapse::fmean(out[[cvars[1L]]][ok], na.rm = TRUE, w = w_ok) -
          collapse::fmean(ale, w = w_ok)
      }
      out$ale[ok] <- ale
    }
  }

  # Convert non-numeric levels *after* calculation of partial dependence and ale!
  if (!num && !is.factor(out$bin_mean)) {
    out$bin_mid <- out$bin_mean <- factor(out$bin_mean)
  }
  return(out)
}

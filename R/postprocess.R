#' Postprocess Marginal Object
#'
#' This function helps to improve the output of [marginal()],
#' [partial_dependence()], [average_observed()]. Except for `sort` and `drop_stats`,
#' all arguments are vectorized, i.e., you can pass a vector or list of the same
#' length as `x`.
#'
#' @param x Object of class "marginal".
#' @param sort Should `x` be sorted in decreasing order of feature importance?
#'   Importance is measured by the weighted variance of the most
#'   relevant available statistic (pd > pred > obs). The default is `FALSE`.
#'   Importance is calculated after the other postprocessings.
#' @param drop_stats Statistics to drop, by default `NULL`.
#'   Subset of "pred", "obs", "pd". Not vectorized over `x`.
#' @param eval_at_center If `FALSE` (default), the points are aligned with (weighted)
#'   average X values per bin. If `TRUE`, the points are aligned with bar centers.
#'   Since categorical X are always evaluated at bar centers, this only affects
#'   numeric X. Note that partial dependence of numeric X is always evaluated at
#'   bar means, not centers. Vectorized over `x`.
#' @param collapse_m If a categorical X has more than `collapse_m` levels,
#'   rare levels are collapsed into a new level "Other".
#'   By default 30. Set to `Inf` for no collapsing. Vectorized over `x`.
#' @param drop_below_n Drop bins with weight below this value. Applied after the
#'   effect of `collapse_m`. Vectorized over `x`.
#' @param na.rm Should `NA` levels in X be dropped?
#' @seealso [marginal()], [average_observed()], [partial_dependence()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5) |>
#'   postprocess(sort = TRUE, eval_at_center = TRUE) |>
#'   plot(num_points = TRUE)
postprocess <- function(
  x,
  sort = FALSE,
  drop_stats = NULL,
  eval_at_center = FALSE,
  collapse_m = 30L,
  drop_below_n = 0,
  na.rm = FALSE
) {
  stopifnot(
    inherits(x, "marginal"),
    collapse_m >= 2L
  )

  out <- mapply(
    postprocess_one,
    x = x,
    eval_at_center = eval_at_center,
    collapse_m = collapse_m,
    drop_below_n = drop_below_n,
    na.rm = na.rm,
    MoreArgs = list(drop_stats = drop_stats),
    SIMPLIFY = FALSE
  )

  if (isTRUE(sort)) {
    imp <- main_effect_importance(out)
    out <- out[order(imp, decreasing = TRUE, na.last = TRUE)]
  }

  class(out) <- "marginal"
  out
}

postprocess_one <- function(
  x,
  drop_stats,
  collapse_m,
  eval_at_center,
  drop_below_n,
  na.rm
) {
  num <- is.numeric(x$eval_at)
  all_stats <- c("pred", "obs", "pd")

  if (!is.null(drop_stats)) {
    stopifnot(drop_stats %in% all_stats)
    # If "pd" in drop_stats, we also drop the non-existent pd_sd
    x <- x[setdiff(colnames(x), c(drop_stats, paste0(drop_stats, "_sd")))]
  }

  if (num) {
    if (isTRUE(eval_at_center)) {
      x$eval_at <- x$bar_at
    }
  }

  if (!num) {
    if (collapse_m < nrow(x)) {
      x_list <- split(x, order(x$weight, decreasing = TRUE) < collapse_m)
      x_keep <- x_list$`TRUE`
      x_agg <- x_list$`FALSE`

      # Prepare new factors
      x_keep <- droplevels(x_keep)
      lvl <- levels(x_keep$bar_at)
      oth <- make.names(c(lvl, "other"), unique = TRUE)[length(lvl) + 1L]
      levels(x_keep$bar_at) <- levels(x_keep$eval_at) <- c(lvl, oth)

      # Collapse other rows
      M <- x_agg[intersect(colnames(x), all_stats)]
      S <- x_agg[intersect(colnames(x), c("obs_sd", "pred_sd"))]
      w <- x_agg$weight
      x_new <- data.frame(
        bar_at = oth,
        bar_width = 0.7,
        eval_at = oth,
        weight = sum(w),
        collapse::fmean(M, w = w, drop = FALSE),
        sqrt(collapse::fsum(S^2 * (w - 1), drop = FALSE) / (sum(w) - 1))  # OK?
      )
      x <- rbind(x_keep, x_new)  # Column order of x_new does not matter
    }
  }

  if (drop_below_n > 0) {
    x <- subset(x, weight >= drop_below_n)
  }

  if (isTRUE(na.rm)) {
    x <- subset(x, !is.na(bar_at))
  }

  droplevels(x)
}

#' Main Effect Importance
#'
#' Extracts the weighted variances of the most relevant statistic
#' (pd > pred > obs) from a "marginal" object. This serves as a simple "main effect"
#' importance measure.
#'
#' @param x Marginal object.
#' @param statistic The statistic used to calculate the variance for.
#' One of 'pd', 'pred', or 'obs'.
#' @seealso [postprocess()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' M <- marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)
#' main_effect_importance(M)
main_effect_importance <- function(x, statistic = NULL) {
  if (is.null(statistic)) {
    vars <- intersect(c("pd", "pred", "obs"), colnames(x[[1L]]))
    statistic <- vars[1L]
    message("Importance via weighted variance of '", statistic, "'")
  }
  vapply(x, FUN = .one_imp, v = statistic, FUN.VALUE = numeric(1))
}

# Helper function
.one_imp <- function(x, v) {
  ok <- is.finite(x[[v]])
  stats::cov.wt(x[ok, v, drop = FALSE], x[["weight"]][ok], method = "ML")$cov[1L, 1L]
}

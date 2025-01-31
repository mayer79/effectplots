#' Update "EffectData" Object
#'
#' @description
#' Updates an "EffectData" object by

#' - turning discrete values to factor (especially useful with the next option),
#' - collapsing levels of categorical variables with many levels,
#' - dropping empty bins,
#' - dropping small bins,
#' - dropping bins with missing name, or
#' - sorting the variables by their importance, see [effect_importance()]-
#'
#' Except for `sort_by`, all arguments are vectorized, i.e., you can
#' pass a vector or list of the same length as `object`.
#'
#' @param object Object of class "EffectData".
#' @param sort_by By which statistic ("pd", "pred_mean", "y_mean", "resid_mean", "ale")
#'   should the results be sorted? The default is "no" (no sorting). Calculated
#'   after all other update steps, e.g., after collapsing or dropping rare levels.
#' @param to_factor Should discrete features be treated as factors?
#'   In combination with `collapse_m`, this can be used to collapse rare values of
#'   discrete numeric features.
#' @param collapse_m If a factor or character feature has more than `collapse_m` levels,
#'   rare levels are collapsed into a new level "other". Standard deviations are
#'   collapsed via root of the weighted average variances. The default is 30.
#'   Set to `Inf` for no collapsing.
#' @param collapse_by How to determine "rare" levels in `collapse_m`?
#'   Either "weight" (default) or "N". Only matters in situations with case weights `w`.
#' @param drop_empty Drop empty bins. Equivalent to `drop_below_n = 1`.
#' @param drop_below_n Drop bins with N below this value. Applied after collapsing.
#' @param drop_below_weight Drop bins with weight below this value. Applied after
#' collapsing.
#' @param na.rm Should missing bin centers be dropped? Default is `FALSE`.
#' @param ... Currently not used.
#' @returns A modified object of class "EffectData".
#' @seealso
#'   [feature_effects()], [average_observed()], [average_predicted()],
#'   [partial_dependence()], [ale()], [bias()], [effect_importance()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' feature_effects(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5) |>
#'   update(sort = "pd", collapse_m = 2) |>
#'   plot()
update.EffectData <- function(
  object,
  sort_by = c("no", "pd", "pred_mean", "y_mean", "resid_mean", "ale"),
  to_factor = FALSE,
  collapse_m = 30L,
  collapse_by = c("weight", "N"),
  drop_empty = FALSE,
  drop_below_n = 0,
  drop_below_weight = 0,
  na.rm = FALSE,
  ...
) {
  sort_by <- match.arg(sort_by)
  collapse_by <- match.arg(collapse_by)
  stopifnot(collapse_m >= 2L)

  out <- mapply(
    update_one,
    x = object,
    to_factor = to_factor,
    collapse_m = collapse_m,
    collapse_by = collapse_by,
    drop_empty = drop_empty,
    drop_below_n = drop_below_n,
    drop_below_weight = drop_below_weight,
    na.rm = na.rm,
    SIMPLIFY = FALSE
  )
  class(out) <- "EffectData"

  if (sort_by == "no") out else  out[order(-effect_importance(out, by = sort_by))]
}

# Helper functions
update_one <- function(
    x,
    to_factor,
    collapse_m,
    collapse_by,
    drop_empty,
    drop_below_n,
    drop_below_weight,
    na.rm
) {
  discrete <- is_discrete(x)
  if (to_factor && discrete && !is.factor(x$bin_mid)) {
    x$bin_mid <- x$bin_mean <- factor(x$bin_mean)
  }
  if (drop_empty) {
    x <- droplevels(subset(x, N > 0L))
  }
  if ((is.factor(x$bin_mid) || is.character(x$bin_mid)) && collapse_m < nrow(x)) {
    x <- .collapse_m(x, m = collapse_m, by = collapse_by)
  }
  if (drop_below_n > 0) {
    x <- droplevels(subset(x, N >= drop_below_n))
  }
  if (drop_below_weight > 0) {
    x <- droplevels(subset(x, weight >= drop_below_weight))
  }
  if (isTRUE(na.rm)) {
    x <- subset(x, !is.na(bin_mid))
  }
  attr(x, "discrete") <- discrete
  return(x)
}

.collapse_m <- function(x, m, by) {
  stopifnot(
    m >= 2L,
    m < nrow(x),
    is.factor(x$bin_mid) || is.character(x$bin_mid)
  )

  # Split into large and small levels
  ind <- order(x[[by]], decreasing = TRUE)
  x_keep <- x[sort(ind[1:(m - 1L)]), ]  # largest m - 1 categories in original order
  x_agg <- x[ind[m:nrow(x)], ]          # the rest

  # Derive name of new value: "other" or "other1" etc.
  fact <- is.factor(x_keep$bin_mid)
  if (fact) {
    x_keep <- droplevels(x_keep)
  }
  lvl <- stats::na.omit(as.character(x_keep$bin_mid))
  oth <- paste("other", nrow(x_agg))
  if (fact) {
    levels(x_keep$bin_mid) <- levels(x_keep$bin_mean) <- c(lvl, oth)
  }

  # Collapse other rows
  w <- x_agg$weight
  x_new <- data.frame(
    bin_mid = oth, bin_width = 0.7, bin_mean = oth, N = sum(x_agg$N), weight = sum(w)
  )

  m_cols <- intersect(colnames(x), c("pred_mean", "y_mean", "resid_mean", "pd", "ale"))
  if (length(m_cols)) {
    x_new[, m_cols] <- collapse::fmean(x_agg[m_cols], w = w, drop = FALSE, na.rm = TRUE)
  }

  s_cols <- intersect(colnames(x), c("pred_sd", "y_sd", "resid_sd"))
  if (length(s_cols)) {
    x_new[, s_cols] <- sqrt(
      collapse::fmean(x_agg[s_cols]^2, w = w, drop = FALSE, na.rm = TRUE)
    )
  }
  rbind(x_keep, x_new)  # Column order of x_new does not matter
}

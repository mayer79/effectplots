#' Update Marginal Object
#'
#' @description
#' Updates a "marginal" object by
#' - sorting the variables by their importance, see [ep_importance()],
#' - collapsing levels of categorical variables with many levels,
#' - dropping small bins, or
#' - dropping bins with missing name.
#'
#' Except for `sort_by`, all arguments are vectorized, i.e., you can
#' pass a vector or list of the same length as `object`.
#'
#' @param object Object of class "marginal".
#' @param sort_by By which statistic ("pd", "pred_mean", "y_mean", "resid_mean", "ale")
#'   should the results be sorted? The default is "no" (no sorting). Calculated
#'   after all other update steps, e.g., after collapsing or dropping rare levels.
#' @param collapse_m If a categorical X has more than `collapse_m` levels,
#'   rare levels are collapsed into a new level "Other". Standard deviations are
#'   collapsed via root of the weighted average variances. The default is 30.
#'   Set to `Inf` for no collapsing.
#' @param collapse_by How to determine "rare" levels in `collapse_m`?
#'   Either "weight" (default) or "N". Only matters in situations with case weights `w`.
#' @param drop_below_n Drop bins with N below this value. Applied after collapsing.
#' @param drop_below_weight Drop bins with weight below this value. Applied after
#' collapsing.
#' @param na.rm Should missing bin centers be dropped? Default is `FALSE`.
#' @seealso
#'   [marginal()], [average_observed()], [average_predicted()],
#'   [partial_dependence()], [ale()], [ep_importance()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5) |>
#'   update(sort = "pd", collapse_m = 2) |>
#'   plot()
update.marginal <- function(
  object,
  sort_by = c("no", "pd", "pred_mean", "y_mean", "resid_mean", "ale"),
  collapse_m = 30L,
  collapse_by = c("weight", "N"),
  drop_below_n = 0,
  drop_below_weight = 0,
  na.rm = FALSE
) {
  sort_by <- match.arg(sort_by)
  collapse_by <- match.arg(collapse_by)
  stopifnot(collapse_m >= 2L)

  out <- mapply(
    update_one,
    x = object,
    collapse_m = collapse_m,
    collapse_by = collapse_by,
    drop_below_n = drop_below_n,
    drop_below_weight = drop_below_weight,
    na.rm = na.rm,
    SIMPLIFY = FALSE
  )
  class(out) <- "marginal"

  if (sort_by == "no") out else  out[order(-ep_importance(out, by = sort_by))]
}

# Helper functions
update_one <- function(
    x, collapse_m, collapse_by, drop_below_n, drop_below_weight, na.rm
) {
  if (!.num(x) && collapse_m < nrow(x)) {
    x <- .collapse_m(x, m = collapse_m, by = collapse_by)
  }
  if (drop_below_n > 0) {
    x <- subset(x, N >= drop_below_n)
  }
  if (drop_below_weight > 0) {
    x <- subset(x, weight >= drop_below_weight)
  }
  if (isTRUE(na.rm)) {
    x <- subset(x, !is.na(bin_mid))
  }
  return(droplevels(x))
}

.collapse_m <- function(x, m, by) {
  x_list <- split(x, order(x[[by]], decreasing = TRUE) < m)
  x_keep <- x_list$`TRUE`
  x_agg <- x_list$`FALSE`

  # Prepare new factors
  x_keep <- droplevels(x_keep)
  lvl <- levels(x_keep$bin_mid)
  oth <- make.names(c(lvl, "other"), unique = TRUE)[length(lvl) + 1L]
  levels(x_keep$bin_mid) <- levels(x_keep$bin_mean) <- c(lvl, oth)

  # Collapse other rows
  w <- x_agg$weight
  x_new <- data.frame(
    bin_mid = oth, bin_width = 0.7, bin_mean = oth, N = sum(x_agg$N), weight = sum(w)
  )

  m_cols <- intersect(colnames(x), c("pred_mean", "y_mean", "resid_mean", "pd", "ale"))
  if (length(m_cols)) {
    x_new[, m_cols] <- collapse::fmean(x_agg[m_cols], w = w, drop = FALSE)
  }

  s_cols <- intersect(colnames(x), c("pred_sd", "y_sd", "resid_sd"))
  if (length(s_cols)) {
    # Bins with N = 1 can be NA, therefore na.rm = TRUE
    x_new[, s_cols] <- sqrt(
      collapse::fmean(x_agg[s_cols]^2, w = w, drop = FALSE, na.rm = TRUE)
    )
  }
  rbind(x_keep, x_new)  # Column order of x_new does not matter
}

#' Postprocess Output of marginal()
#'
#' This function offers different ways to improve the output of [marginal()],
#' [partial_dependence()], [average_observed()].
#'
#' @param object Object of class "marginal" or "multimarginal".
#' @param cat_drop_below_n Drop categories with exposure below this value.
#' @param cat_drop_below_prop Drop categories with relative exposure below this value.
#' @param cat_explicit_na Should `NA` levels in categoricals be converted to strings?
#' @param drop_na Should `NA` levels be dropped?
#' @param ... Currently unused.
#' @export
postprocess <- function(object, ...) {
  UseMethod("postprocess")
}

#' @describeIn postprocess Default method.
#' @export
postprocess.default <- function(object, ...) {
  stop("Undefined")
}

#' @describeIn postprocess Method for "marginal" object.
#' @export
postprocess.marginal <- function(
    object,
    cat_drop_below_n = 0,
    cat_drop_below_prop = 0,
    cat_explicit_na = TRUE,
    drop_na = FALSE,
    ...
) {
  X <- object$data

  if (!object$num) {
    if (cat_drop_below_n > 0) {
      X <- X[X$exposure >= cat_drop_below_n, ]
    }
    if (cat_drop_below_prop > 0) {
      X <- X[X$exposure / sum(X$exposure) >= cat_drop_below_prop, ]
    }

    if (cat_explicit_na) {
      s <- is.na(X$bar_at)
      if (any(s)) {
        lvl <- levels(X$bar_at)
        if ("NA" %in% lvl) {
          stop("'bar_at' contains level 'NA'. This is incompatible with missing handling.")
        }
        levels(X$bar_at) <- levels(X$eval_at) <- c(lvl, "NA")
        X[s, c("bar_at", "eval_at")] <- "NA"
      }
    }
    X <- droplevels(X)
  } else {
    if (isTRUE(drop_na)) {
      X <- X[!is.na(X$bar_at), ]
    }
  }

  object$data <- X
  return(object)
}

#' @describeIn postprocess Method for "multimarginal" object.
#' @export
postprocess.multimarginal <- function(object, ...) {
  out <- lapply(object, postprocess, ...)
  class(out) <- "multimarginal"
  out
}

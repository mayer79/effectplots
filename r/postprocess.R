#' @export
postprocess <- function(object, ...) {
  UseMethod("postprocess")
}

#' @export
postprocess.default <- function(object, ...) {
  stop("Undefined")
}

#' @export
postprocess.marginal <- function(
    object,
    disc_drop_below_n = 0,
    disc_drop_below_prop = 0,
    disc_explicit_na = TRUE,
    drop_na = FALSE
) {
  X <- object$data

  if (object$discrete) {
    if (disc_drop_below_n > 0) {
      X <- X[X$exposure >= drop_below_n, ]
    }
    if (disc_drop_below_prop > 0) {
      X <- X[X$exposure / sum(X$exposure)>= drop_below_prop, ]
    }

    if (disc_explicit_na) {
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

#' @export
postprocess.multimarginal <- function(object, ...) {
  out <- lapply(object, postprocess)
  class(out) <- "multimarginal"
}

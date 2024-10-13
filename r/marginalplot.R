#' Marginal Statistics
#'
#' Calculates
#' - average observed,
#' - average predicted,
#' - partial dependence, and
#' - counts/weights
#' over a (binned) feature v, possibly weighted.
#'
#'
#'
#' @param object Fitted model.
#' @param x_name Column name of the stratification variable shown on the x axis.
#' @param data Matrix-like.
#' @param y Numeric vector of the response. Can also be a column name in `data`.
#'   Omitted if `NULL` (default).
#' @param pred Numeric vector with predictions. If `NULL` (and `pref_fun` is not
#'   `NULL`), it is calculated as `pred_fun(object, data, ...)`. Used to save time
#'   if the function is to be called multiple times. Can be omitted only if both
#'   `pred` and `pred_fun` are `NULL`, which also excludes the possibility to crunch
#'   partial dependence.
#' @param pred_fun Prediction function, by default `stats::predict`. The function takes
#'   three arguments: `object`, `data`, and `...`.
#' @param w Optional vector with case weights. Can also be a column name in `data`.
#' @param breaks An integer, a vector, a string or a function specifying the bins,
#'   see [graphics::hist()]. The default is "Sturges". Calculations are done by
#'   [graphics::hist()].
#' @param right Should bins created via [graphics::hist()] be right-closed?
#'   The default is `TRUE`.
#' @param discrete Set to `TRUE` to force a numeric x to be treated as discrete, i.e.,
#'   not to use binning.
#' @param pd_n Size of the dataset used for calculation of partial dependence.
#'   The default is 500. Set to 0 (or pass `pred_fun = NULL`) to omit calculation
#'   of partial dependence.
#' @param ... Further arguments passed to `pred_fun()`, e.g., `type = "response"` in
#'   a `glm()` model.
#' @returns
#'   An object of class "marginal" containing these elements:
#'   - `data`: data.frame containing the partial dependencies.
#'   - `v`: Same as input `v`.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `by_name`: Column name of grouping variable (or `NULL`).
#' @references
#'   Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."*
#'     Annals of Statistics 29, no. 5 (2001): 1189-1232.
#' @export
#' @examples
#' library(ranger)
#'
#' fit <- ranger(Sepal.Length ~ ., data = iris)
#' pf <- function(m, x) predict(m, x)$predictions
#'
#' M <- marginal.default(
#'   fit,
#'   x_name = "Species",
#'   data = iris,
#'   y = "Sepal.Length",
#'   pred_fun = pf
#' )
#' M
#' M |> plot(backend = "plotly")
#' M |> plot()
marginal <- function(object, ...) {
  UseMethod("marginal")
}

#' @describeIn marginal Default method.
#' @export
marginal.default <- function(
    object,
    x_name,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = stats::predict,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete = NULL,
    pd_n = 500L,
    ...
) {
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    is.null(pred_fun) || is.function(pred_fun),
    x_name %in% colnames(data)
  )

  # Prepare pred
  if (is.null(pred) && !is.null(pred_fun)) {
    pred <- prep_vector(pred_fun(object, data, ...))
  } else if (!is.null(pred)) {
    if (length(pred) != nrow(data)) {
      stop("'pred' should be a vector of length nrow(data), or NULL.")
    }
    pred <- prep_vector(pred)
  }

  # Prepare y (can stay NULL)
  if (!is.null(y)) {
    y <- prep_vector(name_or_vector(y, data))
  }

  # Prepare w (can stay NULL)
  if (!is.null(w)) {
    w <- prep_vector(name_or_vector(w, data))
    if (any(w < 0) || anyNA(w)) {
      stop("'w' can't have negative or missing values")
    }
  }

  # calculate_stats() does not work if both pred and y are NULL
  if (is.null(pred) && is.null(y)) {
    stop("Either 'pred' or 'y' must be available.")
  }

  x <- if (is.matrix(data)) data[, x_name] else data[[x_name]]

  out <- calculate_stats(
    x = x,
    pred = pred,
    y = y,
    w = w,
    breaks = breaks,
    right = right,
    discrete = discrete
  )

  if (pd_n >= 1L && !is.null(pred_fun)) {
    out$data$pd <- partial_dep(
      object = object,
      v = x_name,
      X = data,
      grid = out$data$eval_at,
      pred_fun = pred_fun,
      pd_n = pd_n,
      w = w,
      ...
    )
  }

  out$x_name <- x_name
  class(out) <- "marginal"
  out
}

#' @describeIn marginal Method for "ranger" models.
#' @export
# marginal.ranger <- function() {
#   print("Todo")
#   # marginal.default()
# }

#' @describeIn marginal Method for DALEX "explainer".
#' @export
marginal.explainer <- function() {
  print("Todo")
  # marginal.default()
}

#' @export
#' @examples
#' M <- bivariate("Species", data = iris, y = "Sepal.Length")
#' M
#' M |> plot(backend = "plotly")
#' M |> plot()
bivariate <- function(
    x_name, data, y, w = NULL, breaks = "Sturges", right = TRUE, discrete = NULL
) {
  marginal.default(
    object = NULL,
    x_name = x_name,
    data = data,
    y = y,
    pred = NULL,
    pred_fun = NULL,
    w = w,
    breaks = breaks,
    right = right,
    discrete = discrete,
  )
}

#' Prints "marginal" Object
#'
#' Print method for object of class "marginal".
#'
#' @param x An object of class "marginal".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [marginal()] for examples.
print.marginal <- function(x, ...) {
  cat("marginal object: \n", sep = "")
  print(x$data)
  invisible(x)
}


#' Plots "marginal" Object
#'
#' Plots all information calculated from [marginal()] using a color blind palette from
#' {ggthemes}. When {plotly} is installed, you can switch to the interactive interface
#' by setting `backend = "plotly"`. In this case, the result is expected to be better
#' than via `ggplotly()`.
#'
#' Bars can be switched off via `exposure = FALSE`. Single lines can be switched off
#' by passing a shorter `line_colors` vector.
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "marginal".
#' @param line_colors Named vector of line colors. By default, a color blind
#'   palette from {ggthemes} is used, equalling to
#'   `c(obs = "#E69F00", pred = "#009E73", pd = "#56B4E9")`.
#'   To change globally, set `options(marginalplot.line_colors = "named color vector")`.
#'   Can be used to remove certain lines in the plot.
#' @param fill Fill color of bars. The default equals "lightgrey".
#'   To change globally, set `options(marginalplot.fill = "new color")`.
#' @param rotate_x Should x axis labels be rotated by 45 degrees (only ggplot2)?
#' @param show_exposure Should exposure bars be shown (on hidden right y axis)?
#'   For `backend = "ggplot"`, a value between 0 and 1 scales the bar heights so they
#'   would cover only part of the y range.
#' @param drop_below Drop values with lower exposure. The default 0 keeps all.
#' @param na.rm Should values for missing v be plotted (on the very right)?
#'   Default is `TRUE`.
#' @param backend Plotting backend, one of "ggplot2" (default) or "plotly".
#'   To change globally, set `options(marginalplot.backend = "plotly")`.
#' @param ... Currently not used.
#' @export
#' @returns An object of class "ggplot" or "plotly"/"htmlwidget".
plot.marginal <- function(
    x,
    line_colors = getOption("marginalplot.line_colors"),
    fill = getOption("marginalplot.fill"),
    rotate_x = FALSE,
    show_exposure = TRUE,
    drop_below = 0,
    na.rm = FALSE,
    backend = getOption("marginalplot.backend"),
    ...
) {
  stopifnot(backend %in% c("ggplot2", "plotly"))
  vars_to_show <- Reduce(
    intersect, list(c("obs", "pred", "pd"), colnames(x$data), names(line_colors))
  )

  if (drop_below > 0) {
    x$data <- x$data[x$data$exposure >= drop_below, ]
  }
  if (isTRUE(na.rm)) {
    x$data <- x$data[!is.na(x$data$bar_at), ]
  }

  if (backend == "plotly") {
    p <- plot_marginal_plotly(
      x,
      vars_to_show = vars_to_show,
      line_colors = line_colors,
      fill = fill,
      show_exposure = show_exposure
    )
    return(p)
  }
  plot_marginal_ggplot(
    x,
    vars_to_show = vars_to_show,
    line_colors = line_colors,
    fill = fill,
    rotate_x = rotate_x,
    show_exposure = show_exposure,
    ...
  )
}

plot_marginal_plotly <- function(
    x, vars_to_show, line_colors, fill, show_exposure, ...
) {
  fig <- plotly::plot_ly()

  if (show_exposure) {
    fig <- plotly::add_bars(
      fig,
      x = ~bar_at,
      y = ~exposure,
      yaxis = "y2",
      color = I(fill),
      name = "exposure",
      showlegend = FALSE,
      width = ~bar_width,
      marker = list(line = list(color = fill, width = 1)),  # to avoid tiny white gaps
      data = x$data
    )
  }

  for (z in vars_to_show) {
    fig <- plotly::add_trace(
      fig,
      x = ~eval_at,
      y = x$data[[z]],
      mode = "lines+markers",
      type = "scatter",
      name = z,
      color = I(line_colors[z])
    )
  }

  plotly::layout(
    fig,
    yaxis2 = list(side = "right", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "Response", overlaying = "y2"),
    xaxis = list(title = x$x_name),
    legend = list(orientation = "v", x = 1.05, xanchor = "left")
  )
}

plot_marginal_ggplot <- function(
    x,
    vars_to_show,
    line_colors,
    fill,
    rotate_x,
    show_exposure,
    ...
) {
  df <- poor_man_stack(x$data, vars_to_show)

  if (show_exposure > 0) {
    mult <- show_exposure * diff(range(df$value_, na.rm = TRUE)) / max(x$data$exposure)
    add <- min(df$value_, na.rm = TRUE)

    bars <- ggplot2::geom_tile(
      x$data,
      mapping = ggplot2::aes(
        x = bar_at,
        y = exposure / 2 * mult + add,
        height = exposure * mult,
        width = bar_width
      ),
      show.legend = FALSE,
      fill = fill
    )
    # ggplot2::scale_y_continuous(
    # sec.axis = ggplot2::sec_axis(
    #   transform = ~ (. - add) / mult, name = ggplot2::element_blank()
    # ))
  } else {
    bars <- NULL
  }

  p <- ggplot2::ggplot(df, aes(x = eval_at, y = value_)) +
    bars +
    ggplot2::geom_point(ggplot2::aes(color = varying_), size = 2) +
    ggplot2::geom_line(
      ggplot2::aes(color = varying_, group = varying_), linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(values = line_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "right") +
    ggplot2::labs(x = x$x_name, y = "Prediction scale")

  if (rotate_x) {
    p <- p + rotate_x_labs()
  }
  p
}


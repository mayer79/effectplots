#' Plots "marginal" Object
#'
#' Plots all information calculated from [marginal()] using a color blind palette from
#' "ggthemes". When "plotly" is installed, you can switch to the interactive interface
#' by setting `backend = "plotly"`.
#'
#' Single curves can be switched off by passing a shorter `line_colors` vector.
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "marginal".
#' @param ncols Number of columns in the plot layout.
#'   Only if `length(x) > 1` (multiple plots).
#' @param share_y Should y axis be shared across all subplots?
#'   No effect if `ylim` is passed. Only if `length(x) > 1` (multiple plots).
#' @param sort Should plots be sorted in decreasing order of importance? Importance is
#'   measured by the exposure weighted variance of the most relevant available statistic
#'   (pd > pred > obs). Only if `length(x) > 1` (multiple plots).
#' @param ylim Manual y axis range.
#' @param scale_exposure Scaling of the exposure bars (between 0 and 1).
#'   The default is 1. Set to 0 for no bars. With "plotly", values between 0 and 1 ar
#'   currently not possible.
#' @param line_colors Named vector of line colors. By default, a color blind
#'   palette from "ggthemes" is used, equaling to
#'   `c(obs = "#E69F00", pred = "#009E73", pd = "#56B4E9")`.
#'   To change globally, set `options(marginalplot.line_colors = "named color vector")`.
#'   Can be used to remove certain lines in the plot.
#' @param fill Fill color of bars. The default equals "lightgrey".
#'   To change globally, set `options(marginalplot.fill = "new color")`.
#' @param wrap_x Should categorical x axis labels be wrapped after this length?
#'   The default is 10. Set to 0 for no wrapping. Vectorized over `x`.
#'   Only for "ggplot2" backend.
#' @param rotate_x Should categorical x axis labels be rotated by this angle?
#'   The default is 0 (no rotation). Vectorized over `x`.
#'   Only for "ggplot2" backend.
#' @param backend Plot backend, either "ggplot2" (default) or "plotly".
#'   To change globally, set `options(marginalplot.backend = "plotly")`.
#' @param ... Currently unused.
#' @returns
#'   If `length(x) == 1` (single plot), an object of class  "ggplot" or "plotly".
#'   Otherwise, an object of class "patchwork" or a "plotly" subplot.
#' @seealso [marginal()], [average_observed()], [partial_dependence()]
#' @export
plot.marginal <- function(
    x,
    ncols = 2L,
    share_y = FALSE,
    sort = FALSE,
    ylim = NULL,
    scale_exposure = 1,
    line_colors = getOption("marginalplot.line_colors"),
    fill = getOption("marginalplot.fill"),
    wrap_x = 10,
    rotate_x = 0,
    backend = getOption("marginalplot.backend"),
    ...
) {
  stopifnot(backend %in% c("ggplot2", "plotly"))

  vars_to_show <- Reduce(
    intersect, list(c("obs", "pred", "pd"), colnames(x[[1L]]), names(line_colors))
  )

  if (length(x) == 1L) {
    if (backend == "plotly") {
      p <- plot_marginal_plotly(
        x[[1L]],
        v = names(x),
        vars_to_show = vars_to_show,
        ylim = ylim,
        scale_exposure = scale_exposure,
        line_colors = line_colors,
        fill = fill,
        ...
      )
    }
    p <- plot_marginal_ggplot(
      x[[1L]],
      v = names(x),
      vars_to_show = vars_to_show,
      ylim = ylim,
      scale_exposure = scale_exposure,
      line_colors = line_colors,
      fill = fill,
      wrap_x = wrap_x,
      rotate_x = rotate_x,
      ...
    )
    return(p)
  }

  # Now the multi-plot case
  if (isTRUE(sort)) {
    sorter <- vars_to_show[length(vars_to_show)]
    message("Sorting via exposure weighted variance of '", sorter, "'")
    imp <- vapply(x, FUN = .one_imp, v = sorter, FUN.VALUE = numeric(1))
    x <- x[order(imp, decreasing = TRUE, na.last = TRUE)]
  }

  ncols <- min(ncols, length(x))
  col_i <- (seq_along(x) - 1L) %% ncols + 1L
  row_i <- ceiling(seq_along(x) / ncols)

  if (share_y && is.null(ylim)) {
    r <- range(sapply(x, function(z) range(z[vars_to_show], na.rm = TRUE)))
    ylim <- r + c(-0.05, 0.05) * diff(r)
  }

  if (backend == "ggplot2") {
    plot_list <- mapply(
      plot_marginal_ggplot,
      x,
      v = names(x),
      show_ylab = col_i == 1L,
      show_legend = row_i == 1L & col_i == ncols,
      wrap_x = wrap_x,
      rotate_x = rotate_x,
      MoreArgs = list(
        vars_to_show = vars_to_show,
        show_title = TRUE,
        ylim = ylim,
        scale_exposure = scale_exposure,
        line_colors = line_colors,
        fill = fill
      ),
      SIMPLIFY = FALSE
    )
    patchwork::wrap_plots(plot_list, ncol = ncols, guides = "collect", ...)
  } else {
    plot_list <- mapply(
      plot_marginal_plotly,
      x,
      v = names(x),
      show_ylab = col_i == 1L,
      show_legend = row_i == 1L & col_i == ncols,
      overlay = paste0("y", 2 * seq_along(x)),
      MoreArgs = list(
        vars_to_show = vars_to_show,
        show_title = TRUE,
        ylim = ylim,
        scale_exposure = scale_exposure,
        line_colors = line_colors,
        fill = fill
      ),
      SIMPLIFY = FALSE
    )
    plotly::subplot(
      plot_list,
      titleX = TRUE,
      titleY = TRUE,
      nrows = ceiling(length(x) / ncols),
      margin = c(0.03, 0.05, 0.125, 0.05),
      ...
    )
  }
}

plot_marginal_plotly <- function(
    x,
    v,
    vars_to_show,
    ylim = NULL,
    scale_exposure = 1,
    line_colors,
    fill,
    show_title = FALSE,
    show_ylab = TRUE,
    show_legend = TRUE,
    overlay = "y2",
    ...
) {
  fig <- plotly::plot_ly()

  if (scale_exposure > 0) {
    fig <- plotly::add_bars(
      fig,
      x = ~bar_at,
      y = ~exposure,
      width = ~bar_width,
      data = x,
      yaxis = "y2",
      color = I(fill),
      name = "exposure",
      showlegend = FALSE,
      marker = list(line = list(color = fill, width = 1))  # to remove tiny white gaps
    )
  }

  for (z in vars_to_show) {
    fig <- plotly::add_trace(
      fig,
      x = ~eval_at,
      y = x[[z]],
      data = x,
      yaxis = "y",
      mode = "lines+markers",
      type = "scatter",
      name = z,
      showlegend = show_legend,
      color = I(line_colors[z])
    )
  }

  if (!is.null(ylim)) {
    fig <- plotly::layout(fig, yaxis = list(range = ylim))
  }
  if (show_title) {
    ann <- list(
      text = v,
      x = 0.05,
      y = 1.02,
      font = list(size = 16),
      xanchor = "left",
      yanchor = "bottom",
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    )
    fig <- plotly::layout(fig, annotations = ann)
  }
  plotly::layout(
    fig,
    yaxis = list(
      side = "left",
      title = if (show_ylab) "Response" else "",
      overlaying = overlay,
      zeroline = FALSE
    ),
    yaxis2 = list(side = "right", showgrid = FALSE, showticklabels = FALSE),
    xaxis = list(title = v),
    legend = list(orientation = "v", x = 1.05, xanchor = "left")
  )
}

plot_marginal_ggplot <- function(
    x,
    v,
    vars_to_show,
    ylim = NULL,
    scale_exposure = 1,
    line_colors,
    fill,
    wrap_x = 10,
    rotate_x = 0,
    show_title = FALSE,
    show_ylab = TRUE,
    ...
) {
  df <- poor_man_stack(x, vars_to_show)

  # Calculate transformation of exposure bars on the right y axis
  if (is.null(ylim)) {
    r <- range(df$value_, na.rm = TRUE)
    r <- r + c(-0.03, -0.03) * diff(r)
  } else {
    r <- ylim + c(0.05, -0.05) * diff(ylim)  # ~remove 5% expansion
  }

  if (scale_exposure > 0) {
    mult <- scale_exposure * diff(r) / max(x$exposure)
    bars <- ggplot2::geom_tile(
      x,
      mapping = ggplot2::aes(
        x = bar_at,
        y = exposure / 2 * mult + r[1L],
        height = exposure * mult,
        width = bar_width
      ),
      show.legend = FALSE,
      fill = fill
    )
  } else {
    bars <- NULL
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = eval_at, y = value_)) +
    bars +
    ggplot2::geom_point(
      ggplot2::aes(color = varying_),
      size = 2,
      show.legend = length(vars_to_show) > 1L
    ) +
    ggplot2::geom_line(
      ggplot2::aes(color = varying_, group = varying_),
      linewidth = 0.8,
      show.legend = length(vars_to_show) > 1L
    ) +
    ggplot2::scale_color_manual(values = line_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::labs(
      x = v, y = if (show_ylab) "Response" else ggplot2::element_blank()
    )

  if (show_title) {
    p <- p + ggplot2::ggtitle(v)
  }
  if (!is.null(ylim)) {
    p <- p + ggplot2::ylim(ylim)
  }
  if (!is.numeric(x$eval_at)) {
    if (wrap_x > 0 && is.finite(wrap_x)) {
      p <- p + ggplot2::scale_x_discrete(labels = ggplot2::label_wrap_gen(wrap_x))
    }
    if (rotate_x != 0) {
      p <- p + ggplot2::guides(x = ggplot2::guide_axis(angle = rotate_x))
    }
  }
  p
}

# Helper function
.one_imp <- function(x, v) {
  ok <- is.finite(x[[v]])
  stats::cov.wt(x[ok, v, drop = FALSE], x[["exposure"]][ok], method = "ML")$cov[1L, 1L]
}

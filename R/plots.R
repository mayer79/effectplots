#' Plots "marginal" Object
#'
#' Plots all statistics using a color blind palette from "ggthemes".
#' You can switch to the interactive interface by setting `backend = "plotly"`.
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "marginal".
#' @param ncols Number of columns in the plot layout.
#'   Only if `length(x) > 1` (multiple plots). With "plotly" subplots, only nrow can
#'   be set. Therefore, the calculation is approximately possible only.
#' @param share_y Should y axis be shared across all subplots?
#'   No effect if `ylim` is passed. Only if `length(x) > 1` (multiple plots).
#' @param sort Should plots be sorted in decreasing order of importance? Importance is
#'   measured by the exposure weighted variance of the most relevant available statistic
#'   (pd > pred > obs). Only if `length(x) > 1` (multiple plots).
#' @param ylim Manual y axis range.
#' @param scale_exposure Vertical scaling of the exposure bars (between 0 and 1).
#'   The default is 1. Set to 0 for no bars. With "plotly", values between 0 and 1 ar
#'   currently not possible.
#' @param cat_lines Show lines for non-numeric features. Default is `TRUE`.
#'   Vectorized over `x`.
#' @param num_points Show points for numeric features. Default is `FALSE`.
#'   Vectorized over `x`.
#' @param colors Line colors. By default, a color blind friendly
#'   palette from "ggthemes", namely `c("#CC79A7", "#009E73", "#56B4E9")`.
#'   To change globally, set `options(marginalplot.colors = new colors)`.
#' @param fill Fill color of bars. The default equals "lightgrey".
#'   To change globally, set `options(marginalplot.fill = new color)`.
#' @param wrap_x Should categorical xaxis labels be wrapped after this length?
#'   The default is 10. Set to 0 for no wrapping. Vectorized over `x`.
#'   Only for "ggplot2" backend.
#' @param rotate_x Should categorical xaxis labels be rotated by this angle?
#'   The default is 0 (no rotation). Vectorized over `x`. Only for "ggplot2" backend.
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
    cat_lines = TRUE,
    num_points = FALSE,
    colors = getOption("marginalplot.colors"),
    fill = getOption("marginalplot.fill"),
    wrap_x = 10,
    rotate_x = 0,
    backend = getOption("marginalplot.backend"),
    ...
) {
  vars_to_show <- intersect(c("obs", "pred", "pd"), colnames(x[[1L]]))
  show_legend <- (length(vars_to_show) > 1L)

  stopifnot(
    backend %in% c("ggplot2", "plotly"),
    length(colors) >= length(vars_to_show)
  )


  if (length(x) == 1L) {
    if (backend == "ggplot2") {
      p <- plot_marginal_ggplot(
        x[[1L]],
        v = names(x),
        vars_to_show = vars_to_show,
        ylim = ylim,
        scale_exposure = scale_exposure,
        cat_lines = cat_lines,
        num_points = num_points,
        colors = colors,
        fill = fill,
        wrap_x = wrap_x,
        rotate_x = rotate_x,
        show_legend = show_legend,
        ...
      )
    } else {
      p <- plot_marginal_plotly(
        x[[1L]],
        v = names(x),
        vars_to_show = vars_to_show,
        ylim = ylim,
        scale_exposure = scale_exposure,
        cat_lines = cat_lines,
        num_points = num_points,
        colors = colors,
        fill = fill,
        show_legend = show_legend,
        ...
      )
    }
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
  show_legend <- show_legend & row_i == 1L & col_i == ncols

  if (share_y && is.null(ylim)) {
    r <- range(sapply(x, function(z) range(z[vars_to_show], na.rm = TRUE)))
    ylim <- grDevices::extendrange(r)
  }

  if (backend == "ggplot2") {
    plot_list <- mapply(
      plot_marginal_ggplot,
      x,
      v = names(x),
      show_ylab = col_i == 1L,
      show_legend = show_legend,
      cat_lines = cat_lines,
      num_points = num_points,
      wrap_x = wrap_x,
      rotate_x = rotate_x,
      MoreArgs = list(
        vars_to_show = vars_to_show,
        show_title = TRUE,
        ylim = ylim,
        scale_exposure = scale_exposure,
        colors = colors,
        fill = fill
      ),
      SIMPLIFY = FALSE
    )
    patchwork::wrap_plots(plot_list, ncol = ncols, ...)
  } else {
    plot_list <- mapply(
      plot_marginal_plotly,
      x,
      v = names(x),
      show_ylab = col_i == 1L,
      show_legend = show_legend,
      overlay = paste0("y", 2 * seq_along(x)),
      cat_lines = cat_lines,
      num_points = num_points,
      MoreArgs = list(
        vars_to_show = vars_to_show,
        show_title = TRUE,
        ylim = ylim,
        scale_exposure = scale_exposure,
        colors = colors,
        fill = fill
      ),
      SIMPLIFY = FALSE
    )
    plotly::subplot(
      plot_list,
      titleX = TRUE,
      titleY = TRUE,
      nrows = ceiling(length(x) / ncols),
      margin = c(0.03, 0.05, 0.125, 0.03),
      ...
    )
  }
}

plot_marginal_ggplot <- function(
    x,
    v,
    vars_to_show,
    ylim,
    scale_exposure,
    num_points,
    cat_lines,
    colors,
    fill,
    wrap_x,
    rotate_x,
    show_title = FALSE,
    show_legend = TRUE,
    show_ylab = TRUE,
    ...
) {
  num <- is.numeric(x$eval_at)
  df <- poor_man_stack(x, vars_to_show)

  # Calculate transformation of exposure bars on the right y axis
  if (is.null(ylim)) {
    r <- grDevices::extendrange(df$value_, f = 0.03)
  } else {
    r <- grDevices::extendrange(ylim, f = -0.05)  # ~remove 5% expansion
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = eval_at, y = value_))

  # Add optional exposure bars on secondary y axis
  if (scale_exposure > 0) {
    mult <- scale_exposure * diff(r) / max(x$exposure)

    p <- p + ggplot2::geom_tile(
      x,
      mapping = ggplot2::aes(
        x = bar_at,
        y = exposure / 2 * mult + r[1L],
        height = exposure * mult,
        width = bar_width
      ),
      show.legend = FALSE,
      fill = fill,
      color = fill
    )
  }

  # Add optional points
  if (!num || isTRUE(num_points)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = varying_), size = 2, show.legend = show_legend
    )
  }

  # Add optional lines
  if (num || isTRUE(cat_lines)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(color = varying_, group = varying_),
      linewidth = 0.8,
      show.legend = show_legend
    )
  }

  # Styling
  p <- p + ggplot2::scale_color_manual(values = colors) +
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
  if (!num) {
    if (wrap_x > 0 && is.finite(wrap_x)) {
      p <- p + ggplot2::scale_x_discrete(labels = ggplot2::label_wrap_gen(wrap_x))
    }
    if (rotate_x != 0) {
      p <- p + ggplot2::guides(x = ggplot2::guide_axis(angle = rotate_x))
    }
  }
  p
}

plot_marginal_plotly <- function(
    x,
    v,
    vars_to_show,
    ylim,
    scale_exposure,
    cat_lines,
    num_points,
    colors,
    fill,
    show_title = FALSE,
    show_ylab = TRUE,
    show_legend = TRUE,
    overlay = "y2",
    ...
) {
  num <- is.numeric(x$eval_at)

  if (num && isFALSE(num_points)) {
    scatter_mode <- "lines"
  } else if (!num && isFALSE(cat_lines)) {
    scatter_mode <- "markers"
  } else {
    scatter_mode <-  "lines+markers"
  }

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

  for (i in seq_along(vars_to_show)) {
    z <- vars_to_show[i]
    fig <- plotly::add_trace(
      fig,
      x = ~eval_at,
      y = x[[z]],
      data = x,
      yaxis = "y",
      mode = scatter_mode,
      type = "scatter",
      name = z,
      showlegend = show_legend,
      color = I(colors[i])
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

# Helper function
.one_imp <- function(x, v) {
  ok <- is.finite(x[[v]])
  stats::cov.wt(x[ok, v, drop = FALSE], x[["exposure"]][ok], method = "ML")$cov[1L, 1L]
}

#' Plots "marginal" Object
#'
#' Plots all information calculated from [marginal()] using a color blind palette from
#' {ggthemes}. When {plotly} is installed, you can switch to the interactive interface
#' by setting `backend = "plotly"`.
#'
#' Single lines can be switched off
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
#' @param ylim Manual limits of the y axis range.
#' @param show_exposure Should exposure bars be shown (on hidden right y axis)?
#'   For `backend = "ggplot"`, a value between 0 and 1 scales the bar heights so they
#'   would cover only part of the y range.
#' @param wrap_x Should categorical x axis labels be wrapped after
#'   this number of characters? The default is 10. Only "with ggplot2" backend.
#' @param rotate_x Should categorical x axis labels be rotated by this
#'   number of degrees? Only "with ggplot2" backend. The default is 0 (no rotation).
#' @param backend Plotting backend, one of "ggplot2" (default) or "plotly".
#'   To change globally, set `options(marginalplot.backend = "plotly")`.
#' @param ... Internally used.
#' @export
#' @returns An object of class "ggplot" or "plotly"/"htmlwidget".
plot.marginal <- function(
    x,
    line_colors = getOption("marginalplot.line_colors"),
    fill = getOption("marginalplot.fill"),
    ylim = NULL,
    show_exposure = TRUE,
    wrap_x = 10,
    rotate_x = 0,
    backend = getOption("marginalplot.backend"),
    ...
) {
  stopifnot(backend %in% c("ggplot2", "plotly"))
  vars_to_show <- Reduce(
    intersect, list(c("obs", "pred", "pd"), colnames(x$data), names(line_colors))
  )

  if (backend == "plotly") {
    p <- plot_marginal_plotly(
      x,
      vars_to_show = vars_to_show,
      line_colors = line_colors,
      fill = fill,
      show_exposure = show_exposure,
      ylim = ylim,
      ...
    )
    return(p)
  }
  plot_marginal_ggplot(
    x,
    vars_to_show = vars_to_show,
    line_colors = line_colors,
    fill = fill,
    show_exposure = show_exposure,
    ylim = ylim,
    wrap_x = wrap_x,
    rotate_x = rotate_x,
    ...
  )
}

plot_marginal_plotly <- function(
    x,
    vars_to_show,
    line_colors,
    fill,
    title = NULL,
    show_ylab = TRUE,
    show_legend = TRUE,
    ylim = NULL,
    show_exposure,
    overlay = "y2",
    ...
) {
  fig <- plotly::plot_ly()

  if (show_exposure) {
    fig <- plotly::add_bars(
      fig,
      x = ~bar_at,
      y = ~exposure,
      width = ~bar_width,
      data = x$data,
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
      y = x$data[[z]],
      data = x$data,
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
  if (!is.null(title)) {
    ann <- list(
      text = title,
      x = 0.05,
      y = 1.07,
      font = list(size = 17),
      xanchor = "left",
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    )
    fig <- plotly::layout(fig, annotations = ann)
  }
  plotly::layout(
    fig,
    yaxis = list(
      side = "left", title = if (show_ylab) "Response" else "", overlaying = overlay
    ),
    yaxis2 = list(side = "right", showgrid = FALSE, showticklabels = FALSE),
    xaxis = list(title = x$x_name),
    legend = list(orientation = "v", x = 1.05, xanchor = "left")
  )
}

plot_marginal_ggplot <- function(
    x,
    vars_to_show,
    line_colors,
    fill,
    title = NULL,
    show_ylab = TRUE,
    ylim = NULL,
    show_exposure = TRUE,
    rotate_x = 0,
    wrap_x = 10,
    ...
) {
  df <- poor_man_stack(x$data, vars_to_show)

  # Calculate transformation of exposure bars on the right y axis
  if (is.null(ylim)) {
    r <- range(df$value_, na.rm = TRUE)
    r <- r + c(-0.03, -0.03) * diff(r)
  } else {
    r <- ylim + c(0.05, -0.05) * diff(ylim)  # ~invert typical 5% expansion
  }

  if (show_exposure > 0) {
    mult <- show_exposure * diff(r) / max(x$data$exposure)
    bars <- ggplot2::geom_tile(
      x$data,
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
      x = x$x_name, y = if (show_ylab) "Response" else ggplot2::element_blank()
    )

  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  if (!is.null(ylim)) {
    p <- p + ggplot2::ylim(ylim)
  }
  if (x$discrete) {
    if (wrap_x > 0 && is.finite(wrap_x)) {
      p <- p + ggplot2::scale_x_discrete(labels = ggplot2::label_wrap_gen(wrap_x))
    }
    if (rotate_x != 0) {
      p <- p + ggplot2::guides(x = ggplot2::guide_axis(angle = rotate_x))
    }
  }
  p
}

#' Plots "multimarginal" Object
#'
#' @inheritParams plot.marginal
#' @param share_y Should y axis be shared across all subplots? This has no effect
#'   if `ylim` is provided.
#' @export
plot.multimarginal <- function(
    x,
    line_colors = getOption("marginalplot.line_colors"),
    fill = getOption("marginalplot.fill"),
    ncol = 2L,
    share_y = FALSE,
    ylim = NULL,
    show_exposure = TRUE,
    wrap_x = 10,
    rotate_x = 0,
    backend = getOption("marginalplot.backend"),
    ...
) {

  stopifnot(backend %in% c("ggplot2", "plotly"))
  vars_to_show <- Reduce(
    intersect, list(c("obs", "pred", "pd"), colnames(x[[1L]]$data), names(line_colors))
  )

  ncol <- min(ncol, length(x))
  col_i <- ((seq_len(length(x)) - 1) %% ncol) + 1L
  row_i <- ceiling(seq_along(x) / ncol)

  if (share_y && is.null(ylim)) {
    r <- range(sapply(x, function(z) range(z$data[vars_to_show], na.rm = TRUE)))
    ylim <- r + c(-0.05, 0.05) * diff(r)
  }

  if (backend == "ggplot2") {
    plot_list <- mapply(
      plot.marginal,
      x,
      title = names(x),
      show_ylab = col_i == 1L,
      show_legend = row_i == 1L & col_i == ncol,
      MoreArgs = list(
        line_colors = line_colors,
        fill = fill,
        ylim = ylim,
        show_exposure = show_exposure,
        wrap_x = wrap_x,
        rotate_x = rotate_x,
        backend = backend
      ),
      SIMPLIFY = FALSE
    )
    patchwork::wrap_plots(plot_list, ncol = ncol, guides = "collect", ...)
  } else {  # plotly
    plot_list <- mapply(
      plot.marginal,
      x,
      title = names(x),
      show_ylab = col_i == 1L,
      show_legend = row_i == 1L & col_i == ncol,
      overlay = paste0("y", 2 * seq_along(x)),
      MoreArgs = list(
        line_colors = line_colors,
        fill = fill,
        ylim = ylim,
        show_exposure = show_exposure,
        backend = backend
      ),
      SIMPLIFY = FALSE
    )
    plotly::subplot(
      plot_list,
      titleX = TRUE,
      titleY = TRUE,
      nrows = ceiling(length(x) / ncol),
      margin = c(0.03, 0.05, 0.125, 0.05),
      ...
    )
  }
}


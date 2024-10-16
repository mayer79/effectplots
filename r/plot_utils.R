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
#' @param cat_rotate_x Should x axis labels be rotated by 45 degrees (only ggplot2)?
#' @param show_exposure Should exposure bars be shown (on hidden right y axis)?
#'   For `backend = "ggplot"`, a value between 0 and 1 scales the bar heights so they
#'   would cover only part of the y range.
#' @param backend Plotting backend, one of "ggplot2" (default) or "plotly".
#'   To change globally, set `options(marginalplot.backend = "plotly")`.
#' @param ... Currently not used.
#' @export
#' @returns An object of class "ggplot" or "plotly"/"htmlwidget".
plot.marginal <- function(
    x,
    line_colors = getOption("marginalplot.line_colors"),
    fill = getOption("marginalplot.fill"),
    cat_rotate_x = FALSE,
    show_exposure = TRUE,
    show_legend = NULL,
    ylim = NULL,
    backend = getOption("marginalplot.backend"),
    ...
) {
  stopifnot(backend %in% c("ggplot2", "plotly"))
  vars_to_show <- Reduce(
    intersect, list(c("obs", "pred", "pd"), colnames(x$data), names(line_colors))
  )

  if (is.null(show_legend)) {
    show_legend <- length(vars_to_show) > 1L
  }

  if (backend == "plotly") {
    p <- plot_marginal_plotly(
      x,
      vars_to_show = vars_to_show,
      line_colors = line_colors,
      fill = fill,
      show_exposure = show_exposure,
      show_legend = show_legend,
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
    cat_rotate_x = cat_rotate_x,
    show_exposure = show_exposure,
    show_legend = show_legend,
    ylim = ylim,
    ...
  )
}

plot_marginal_plotly <- function(
    x,
    vars_to_show,
    line_colors,
    fill,
    show_exposure,
    show_legend = NULL,
    ylim = NULL,
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
  plotly::layout(
    fig,
    yaxis = list(side = "left", title = "Response", overlaying = "y2"),
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
    cat_rotate_x,
    show_exposure,
    show_legend = NULL,
    ylim = NULL,
    ...
) {
  df <- poor_man_stack(x$data, vars_to_show)

  if (is.null(ylim)) {
    r <- range(df$value_, na.rm = TRUE)
    r <- r + c(0, -0.03) * diff(r)
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
    # ggplot2::scale_y_continuous(
    # sec.axis = ggplot2::sec_axis(
    #   transform = ~ (. - ylim[1L]) / mult, name = ggplot2::element_blank()
    # ))
  } else {
    bars <- NULL
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = eval_at, y = value_)) +
    bars +
    ggplot2::geom_point(
      ggplot2::aes(color = varying_), size = 2, show.legend = show_legend
    ) +
    ggplot2::geom_line(
      ggplot2::aes(color = varying_, group = varying_),
      linewidth = 0.8,
      show.legend = show_legend
    ) +
    ggplot2::scale_color_manual(values = line_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "right") +
    ggplot2::labs(x = x$x_name, y = "Prediction scale")

  if (!is.null(ylim)) {
    p <- p + ggplot2::ylim(ylim)
  }
  if (cat_rotate_x && x$discrete) {
    p <- p + cat_rotate_x_labs()
  }
  p
}

plot.multimarginal <- function(
    x,
    line_colors = getOption("marginalplot.line_colors"),
    fill = getOption("marginalplot.fill"),
    ncol = 2L,
    share_y = FALSE,
    ylim = NULL,
    cat_rotate_x = FALSE,
    show_exposure = TRUE,
    backend = getOption("marginalplot.backend"),
    ...
) {
  stopifnot(backend %in% c("ggplot2", "plotly"))

  ncol <- min(ncol, length(x))

  col_i <- ((seq_len(length(x)) - 1) %% ncol) + 1L
  row_i <- ceiling(seq_along(x) / ncol)

  if (share_y && backend == "ggplot2" && is.null(ylim)) {
    vars_to_show <- Reduce(
      intersect,
      list(c("obs", "pred", "pd"), colnames(x[[1L]]$data), names(line_colors))
    )
    r <- range(sapply(x, function(z) range(z$data[vars_to_show], na.rm = TRUE)))
    ylim <- r + c(-0.05, 0.05) * diff(r)
  }

  plot_list <- mapply(
    plot.marginal,
    x,
    show_legend = row_i == 1L & col_i == ncol,
    MoreArgs = list(
      line_colors = line_colors,
      fill = fill,
      cat_rotate_x = cat_rotate_x,
      show_exposure = show_exposure,
      ylim = ylim,
      share_y = share_y,
      backend = backend
    ),
    SIMPLIFY = FALSE
  )
  if (backend == "ggplot2") {
    for (i in seq_along(x)) {
      p <- plot_list[[i]]
      p <- p + ggplot2::ggtitle(names(x)[i])
      if (col_i[i] != 1L) {
        p <- p + ggplot2::ylab(element_blank())
      }
      plot_list[[i]] <- p
    }
    patchwork::wrap_plots(plot_list, ncol = ncol)
  } else {
    subplot(
      plot_list,
      titleX = TRUE,
      titleY = TRUE,
      nrows = ceiling(length(x) / ncol),
      margin = 0.05,
      shareY = if (share_y) "all" else FALSE
    )
  }
}


#' Rotate x labels in plots (from hstats)
#'
#' @noRd
#' @keywords internal
#'
#' @returns A theme object.
cat_rotate_x_labs <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )
}


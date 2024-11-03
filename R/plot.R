#' Plots Marginal Object
#'
#' Plots marginal statistics using a color blind palette from "ggthemes".
#' You can switch to plotly by setting `backend = "plotly"`.
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "marginal".
#' @param statistics Vector of (available) statistics to show. By default
#'   `c("y_mean", "pred_mean", "pd", "ale")`. E.g., used to hide certain
#'   statistics, or to show only `"resid_mean"`.
#'   Additionally, it controls the order in which the lines are added to the plot
#'   (the last one is placed on top).
#' @param ncol Number of columns of the plot layout. Only for multiple plots.
#' @param byrow Should plots be placed by row? Default is `TRUE`.
#'   Only for multiple plots.
#' @param share_y Should y axis be shared across all subplots?
#'   No effect if `ylim` is passed. Only for multiple plots.
#' @param ylim A vector of length 2 with manual y axis limits.
#' @param cat_lines Show lines for non-numeric features. Default is `TRUE`.
#' @param num_points Show points for numeric features. Default is `FALSE`.
#' @param title Overall plot title, by default `""` (no title).
#' @param subplot_titles Should variable names be shown as subplot titles?
#'   Default is `TRUE`. Only for multiple plots.
#' @param ylab Label of the y axis. The default `NULL` automatically derives
#'   a reasonable name.
#' @param legend_labels Vector of legend labels corresponding to `statistics`.
#'   Either `NULL` or a vector of the same length as `statistics`.
#' @param colors Vector of line/point colors at least as long as "statistics".
#'   By default, a color blind friendly palette from "ggthemes" with five values.
#'   To change globally, set `options(effectplots.colors = new colors)`.
#' @param fill Fill color of bars. The default equals "lightgrey".
#'   To change globally, set `options(effectplots.fill = new color)`.
#' @param alpha Alpha transparency of lines and points. Default is 1.
#' @param bar_height Relative bar height (default 1). Set to 0 for no bars.
#' @param bar_width Relative bar width of non-numeric features, by default 0.7.
#' @param bar_measure What should bars represent? Either "weight" (default) or "N".
#' @param wrap_x Should categorical x axis labels be wrapped after this length?
#'   The default is 10. Set to 0 for no wrapping. Vectorized over `x`.
#'   Only for "ggplot2" backend.
#' @param rotate_x Should categorical xaxis labels be rotated by this angle?
#'   The default is 0 (no rotation). Vectorized over `x`. Only for "ggplot2" backend.
#' @param backend Plotting backend, either "ggplot2" (default) or "plotly".
#'   To change globally, set `options(effectplots.backend = "plotly")`.
#' @param ... Passed to `patchwork::plot_layout()` or `plotly::subplot()`. Typically
#'   not used.
#' @returns
#'   If a single plot, an object of class  "ggplot" or "plotly".
#'   Otherwise, an object of class "patchwork", or a "plotly" subplot.
#' @seealso [marginal()], [average_observed()], [partial_dependence()]
#' @export
plot.marginal <- function(
    x,
    statistics = c("y_mean", "pred_mean", "pd", "ale"),
    ncol = 2L,
    byrow = TRUE,
    share_y = FALSE,
    ylim = NULL,
    cat_lines = TRUE,
    num_points = FALSE,
    title = "",
    subplot_titles = TRUE,
    ylab = NULL,
    legend_labels = NULL,
    colors = getOption("effectplots.colors"),
    fill = getOption("effectplots.fill"),
    alpha = 1,
    bar_height = 1,
    bar_width = 0.7,
    bar_measure = c("weight", "N"),
    wrap_x = 10,
    rotate_x = 0,
    backend = getOption("effectplots.backend"),
    ...
) {
  bar_measure <- match.arg(bar_measure)

  # Info of the form c(legend label = col name, ...). The order does not matter *yet*.
  stat_info <- c(
    obs = "y_mean", pred = "pred_mean", bias = "resid_mean", pd = "pd", ale = "ale"
  )
  stopifnot(
    backend %in% c("ggplot2", "plotly"),
    length(statistics) >= 1L,
    statistics %in% stat_info,
    is.null(legend_labels) || length(legend_labels) == length(statistics),
    length(colors) >= length(statistics)
  )

  # The next part looks a bit complicated. We build a vector with statistics like "pd"
  # (having legend_labels as names), and a corresponding vector of colors.
  # Afterwards, we don't need legend_labels and statistics anymore.
  stat_info <- stat_info[match(statistics, stat_info)]  # Use order of `statistics`
  if (!is.null(legend_labels)) {
    names(stat_info) <- legend_labels
  }
  stat_info <- stat_info[stat_info %in% colnames(x[[1L]])]  # only *available* stats
  nstat <- length(stat_info)
  stopifnot(nstat >= 1L)
  colors <- colors[seq_len(nstat)]
  # End of complicated part

  # If user manually sets statistics = "ale", we need to drop non-numeric features.
  if (nstat == 1L && stat_info == "ale") {
    ok <- .num(x)
    if (!all(ok)) {
      if (!any(ok)) {
        stop("Nothing to plot!")
      }
      message(
        "Dropping discrete variables (no ALE): ", paste(names(x)[!ok], collapse = ", ")
      )
      x <- x[ok]
    }
  }

  # No legend for single lines
  show_legend <- length(stat_info) > 1L

  # Overwrite bin_width of categorical features
  x <- lapply(x, function(z) {if (!.num(z)) z$bin_width <- bar_width; z})

  # Derive a good ylab
  if (is.null(ylab)) {
    ylab <- get_ylab(stat_info)
  }

  # Finally ready to plot something
  nplots <- length(x)
  if (nplots == 1L) {
    if (backend == "ggplot2") {
      p <- plot_marginal_ggplot(
        x[[1L]],
        v = names(x),
        share_y = FALSE,
        ylim = ylim,
        cat_lines = cat_lines,
        num_points = num_points,
        title = title,
        ylab = ylab,
        stat_info = stat_info,
        colors = colors,
        fill = fill,
        alpha = alpha,
        bar_height = bar_height,
        bar_measure = bar_measure,
        wrap_x = wrap_x,
        rotate_x = rotate_x,
        show_legend = show_legend
      )
    } else {
      p <- plot_marginal_plotly(
        x[[1L]],
        v = names(x),
        share_y = FALSE,
        ylim = ylim,
        cat_lines = cat_lines,
        num_points = num_points,
        title = title,
        title_as_ann = FALSE,
        ylab = ylab,
        stat_info = stat_info,
        colors = colors,
        fill = fill,
        alpha = alpha,
        bar_height = bar_height,
        bar_measure = bar_measure,
        show_legend = show_legend
      )
    }
    return(p)
  }

  # We need some additional preparation with multiple plots
  ncol <- min(ncol, nplots)
  nrow <- ceiling(nplots / ncol)
  if (backend == "plotly") {
    # Plotly uses nrows to set up the layout. Thus, we need to recalculated ncol
    ncol <- ceiling(nplots / nrow)
  }

  # Can be useful if "marginal" objects of different models/datasets are c() together
  # For patchwork, we could also use its plot_layout(byrow = FALSE) argument.
  if (isFALSE(byrow)) {
    # alternating indices, e.g., 1, 4, 2, 5, 3 with ncol = 2 and nplots 0 5
    alt <- c(t(matrix(seq_len(nrow * ncol), ncol = ncol)))[seq_len(nplots)]
    x <- x[alt]
  }

  col_i <- (seq_along(x) - 1L) %% ncol + 1L

  # Shared y is solved via ylim + padding
  if (share_y && is.null(ylim)) {
    r <- common_range(x, stat_info)
    ylim <- grDevices::extendrange(r, f = 0.05)
  }

  if (show_legend) {
    # We prefer a numeric subplot as reference. Placement is done at the right center.
    show_legend <- seq_along(x) == order(!.num(x))[1L]
  }

  titles <- if (isTRUE(subplot_titles)) names(x) else ""

  if (backend == "ggplot2") {
    plot_list <- mapply(
      plot_marginal_ggplot,
      x,
      v = names(x),
      title = titles,
      show_legend = show_legend,
      wrap_x = wrap_x,
      rotate_x = rotate_x,
      MoreArgs = list(
        share_y = share_y,
        ylim = ylim,
        cat_lines = cat_lines,
        num_points = num_points,
        ylab = ylab,
        stat_info = stat_info,
        colors = colors,
        fill = fill,
        alpha = alpha,
        bar_height = bar_height,
        bar_measure = bar_measure
      ),
      SIMPLIFY = FALSE
    )
    p <- patchwork::wrap_plots(plot_list) +
      patchwork::plot_layout(ncol = ncol, guides = "collect", axes = "collect", ...)
    if (title != "") {
      p <- p + patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      )
    }
    p
  } else {
    # Manually reduce horizontal margin and hide right ticks for equal scale
    hide_some_yticks <- isTRUE(share_y) || !is.null(ylim)

    plot_list <- mapply(
      plot_marginal_plotly,
      x,
      v = names(x),
      title = titles,
      show_yticks = (col_i == 1L) | !hide_some_yticks,
      show_legend = show_legend,
      overlay = paste0("y", 2 * seq_along(x)),
      MoreArgs = list(
        title_as_ann = TRUE,
        share_y = share_y,
        ylim = ylim,
        cat_lines = cat_lines,
        num_points = num_points,
        ylab = ylab,
        show_ylab = FALSE,  # replaced by global annotation
        stat_info = stat_info,
        colors = colors,
        fill = fill,
        alpha = alpha,
        bar_height = bar_height,
        bar_measure = bar_measure
      ),
      SIMPLIFY = FALSE
    )
    # left/right - top/bottom (we use it symmetrically)
    margins <- c(0.05 - hide_some_yticks * 0.02, 0.05 + isTRUE(subplot_titles) * 0.02)
    fig <- plotly::subplot(
      plot_list,
      titleX = TRUE,
      titleY = FALSE,
      nrows = nrow,
      widths = corr_margin(ncol, margin = margins[1L]),
      heights = corr_margin(nrow, margin = margins[2L]),
      margin = rep(margins, each = 2L),
      ...
    )

    # Global title (not via layout(title = ))
    if (title != "") {
      fig <- plotly::layout(
        fig, margin = list(t = 40 + 10 * isTRUE(subplot_titles)), title = title
      )
    }

    # Make a single vertical y axis label. xshift does the dynamic shifting
    ann <- list(
      text = ylab,
      x = 0,
      y = 0.5,
      xshift = -50,
      font = list(size = 14),
      textangle = 270,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    )

    plotly::layout(fig, annotations = ann)
  }
}

plot_marginal_ggplot <- function(
    x,
    v,
    share_y,
    ylim,
    num_points,
    cat_lines,
    title,
    ylab,
    stat_info,
    colors,
    fill,
    alpha,
    bar_height,
    bar_measure,
    wrap_x,
    rotate_x,
    show_legend = TRUE
) {
  num <- .num(x)
  if (!num && ("ale" %in% stat_info)) {
    # We don't have ALE for discrete variables. To avoid warnings, we drop it from
    # stat_info and colors. Dito in plotly
    keep <- stat_info != "ale"
    stat_info <- stat_info[keep]
    colors <- colors[keep]
  }
  df <- transform(
    poor_man_stack(x, stat_info),
    varying_ = factor(varying_, levels = stat_info, labels = names(stat_info))
  )

  # Calculate transformation of bars on the right y axis
  if (is.null(ylim)) {
    r <- grDevices::extendrange(df$value_, f = 0.02)
  } else {
    f <- if (isTRUE(share_y)) -0.05 else -0.02
    r <- grDevices::extendrange(ylim, f = f)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = bin_mean, y = value_))

  # Add optional bars on secondary y axis
  if (bar_height > 0) {
    x[["size"]] <- x[[bar_measure]]
    mult <- bar_height * diff(r) / max(x$size)

    p <- p + ggplot2::geom_tile(
      x,
      mapping = ggplot2::aes(
        x = bin_mid,
        y = size / 2 * mult + r[1L],
        height = size * mult,
        width = bin_width
      ),
      show.legend = FALSE,
      fill = fill,
      color = fill
    )
  }

  # Add optional points
  if (!num || isTRUE(num_points)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = varying_),
      size = 2,
      alpha = alpha,
      show.legend = show_legend
    )
  }

  # Add optional lines
  if (num || isTRUE(cat_lines)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(color = varying_, group = varying_),
      linewidth = 0.8,
      alpha = alpha,
      show.legend = show_legend
    )
  }

  # Styling. The color subsetting is due to having dropped "ale" for categoricals
  p <- p + ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "right") +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::labs(x = v, y = ylab)

  if (title != "") {
    p <- p + ggplot2::ggtitle(title)
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
    share_y,
    ylim,
    cat_lines,
    num_points,
    title,
    title_as_ann = FALSE,
    ylab,
    stat_info,
    colors,
    fill,
    alpha,
    bar_height,
    bar_measure,
    show_ylab = TRUE,    # not vectorized
    show_yticks = TRUE,  # vectorized, and all below as well
    show_legend = TRUE,
    overlay = "y2"
) {
  num <- .num(x)
  if (!num && ("ale" %in% stat_info)) {
    # We don't have ALE for discrete variables. To avoid warnings, we drop it from
    # stat_info and colors. Same solution as with ggplot.
    keep <- stat_info != "ale"
    stat_info <- stat_info[keep]
    colors <- colors[keep]
  }

  if (num && isFALSE(num_points)) {
    scatter_mode <- "lines"
  } else if (!num && isFALSE(cat_lines)) {
    scatter_mode <- "markers"
  } else {
    scatter_mode <-  "lines+markers"
  }

  # Deal with NAs in categorical X
  if (!num && anyNA(x$bin_mid)) {
    lvl <- levels(x$bin_mid)
    if ("NA" %in% lvl) {
      warning("Can't show NA level on x axis because there is already a level 'NA'")
    } else {
      levels(x$bin_mid) <- levels(x$bin_mean) <- c(lvl, "NA")
      x[is.na(x$bin_mid), c("bin_mid", "bin_mean")] <- "NA"
    }
  }

  fig <- plotly::plot_ly()

  if (bar_height > 0) {
    fig <- plotly::add_bars(
      fig,
      x = ~bin_mid,
      y = x[[bar_measure]],
      width = ~bin_width,
      data = x,
      yaxis = "y2",
      color = I(fill),
      name = bar_measure,
      showlegend = FALSE,
      marker = list(line = list(color = fill, width = 1))  # to remove tiny white gaps
    )
    # Fix bars slightly above observed maximum value of a line
    f <- if (isFALSE(share_y) && !is.null(ylim)) 1.01 else 0.98
    r <- c(0, max(x[[bar_measure]]) / bar_height / f)
  }

  for (i in seq_along(stat_info)) {
    z <- stat_info[i]
    fig <- plotly::add_trace(
      fig,
      x = ~bin_mean,
      y = x[[z]],
      data = x,
      yaxis = "y",
      mode = scatter_mode,
      type = "scatter",
      name = names(z),
      showlegend = show_legend,
      legendgroup = z,
      color = I(colors[i]),
      opacity = alpha
    )
  }

  if (!is.null(ylim)) {
    fig <- plotly::layout(fig, yaxis = list(range = ylim))
  }
  if (title != "") {
    if (!title_as_ann) {  # Normal title
      fig <- plotly::layout(fig, title = title, margin = list(t = 40))
    } else {  # subplot situation
      ann <- list(
        text = title,
        x = 0,
        y = 1,
        font = list(size = 15),
        xanchor = "left",
        yanchor = "bottom",
        xref = "paper",
        yref = "paper",
        showarrow = FALSE
      )
      fig <- plotly::layout(fig, annotations = ann)
    }
  }

  plotly::layout(
    fig,
    yaxis = list(
      side = "left",
      title = if (show_ylab) ylab else "",
      showticklabels = show_yticks,
      overlaying = overlay,
      zeroline = FALSE
    ),
    yaxis2 = list(
      side = "right",
      showgrid = FALSE,
      showticklabels = FALSE,
      range = if (bar_height > 0) r
    ),
    xaxis = list(title = list(text = v, standoff = 0)),
    legend = list(x = 1.02, y = 0.5, xanchor = "left", tracegroupgap = 3)
  )
}

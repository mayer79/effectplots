#' Plots "EffectData" Object
#'
#' Versatile plot function for an "EffectData" object. By default, all calculated
#' statistics (except "resid_mean") are shown. To select certain statistics,
#' use the `stat` argument. Set `plotly = TRUE` for interactive plots.
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "EffectData".
#' @param stat Vector of statistics to show. The default `NULL` equals either
#'   `c("y_mean", "pred_mean", "pd", "ale")`, or `"resid_mean"`
#'   (when `x` results from [bias()]). Only *available* statistics are shown.
#'   Additionally, this argument controls the order used to plot the lines.
#' @param ncol Number of columns of the plot layout, by default
#'   `grDevices::n2mfrow(length(x))[2L]`. Only relevant for multiple plots.
#' @param byrow Should plots be placed by row? Default is `TRUE`.
#'   Only for multiple plots.
#' @param share_y Should y axis be shared across subplots? The default is "no".
#'   Other choices are "all", "rows", and "cols".
#'   No effect if `ylim` is passed. Only for multiple plots.
#' @param ylim A vector of length 2 with manual y axis limits, or a list thereof.
#' @param cat_lines Show lines for non-numeric features. Default is `TRUE`.
#' @param num_points Show points for numeric features. Default is `FALSE`.
#' @param title Overall plot title, by default `""` (no title).
#' @param subplot_titles Should variable names be shown as subplot titles?
#'   Default is `TRUE`. Only for multiple plots.
#' @param ylab Label of the y axis. The default `NULL` automatically derives
#'   a reasonable name.
#' @param legend_labels Vector of legend labels in the same order as the
#'   statistics plotted, or `NULL` (default).
#' @param colors Vector of line/point colors of sufficient length.
#'   By default, a color blind friendly palette from "ggthemes".
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
#' @param plotly Should 'plotly' be used? The default is `FALSE` ('ggplot2' with
#'   'patchwork'). Use `options(effectplots.plotly = TRUE)` to change globally.
#' @param ... Passed to `patchwork::plot_layout()` or `plotly::subplot()`. Typically
#'   not used.
#' @returns
#'   If a single plot, an object of class  "ggplot" or "plotly".
#'   Otherwise, an object of class "patchwork", or a "plotly" subplot.
#' @seealso [feature_effects()], [average_observed()], [average_predicted()],
#'   [partial_dependence()], [bias()], [ale()]
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' xvars <- colnames(iris)[-1]
#' M <- feature_effects(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)
#' plot(M, share_y = "all")
#' plot(M, stat = c("pd", "ale"), legend_labels = c("PD", "ALE"))
#' plot(M, stat = "resid_mean", share_y = "all")
plot.EffectData <- function(
    x,
    stat = NULL,
    ncol = grDevices::n2mfrow(length(x))[2L],
    byrow = TRUE,
    share_y = c("no", "all", "rows", "cols"),
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
    plotly = getOption("effectplots.plotly"),
    ...
) {
  share_y <- match.arg(share_y)
  bar_measure <- match.arg(bar_measure)

  # Info of the form c(legend label = col name, ...)
  stat_info <- c(
    obs = "y_mean", pred = "pred_mean", bias = "resid_mean", pd = "pd", ale = "ale"
  )

  # Initialize stat
  available <- .stats(x)
  if (is.null(stat)) {
    if (length(available) == 1L && available == "resid_mean") {
      stat <- "resid_mean"
    } else {
      stat <- c("y_mean", "pred_mean", "pd", "ale")
    }
  }
  stat <- intersect(stat, available)
  nstat <- length(stat)

  stopifnot(
    nstat >= 1L,
    stat %in% stat_info,
    length(colors) >= nstat,
    is.null(legend_labels) || length(legend_labels) == nstat,
    is.null(ylim) || is.list(ylim) || (is.numeric(ylim) && length(ylim) == 2L),
    is.null(ylab) || length(ylab) == 1L
  )

  # We filter on "stat", and modify the names via legend_labels. Then, we don't
  # need 'legend_labels' and 'stat' anymore.
  stat_info <- stat_info[match(stat, stat_info)]  # Use order of stat
  if (!is.null(legend_labels)) {
    names(stat_info) <- legend_labels
  }
  colors <- colors[seq_len(nstat)]

  # If user manually sets stat = "ale", we need to drop non-numeric features.
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
  if (bar_width != 0.7) {
    x[] <- lapply(x, function(z) {if (!.num(z)) z$bin_width <- bar_width; z})
  }

  # Derive a good ylab
  if (is.null(ylab)) {
    ylab <- get_ylab(stat_info)
  }

  # Finally ready to plot something
  nplots <- length(x)

  if (nplots == 1L) {
    if (!plotly) {
      p <- one_ggplot(
        x[[1L]],
        v = names(x),
        share_y = "no",
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
      p <- one_plotly(
        x[[1L]],
        v = names(x),
        share_y = "no",
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
  J <- seq_len(nplots)
  if (plotly) {
    # Plotly uses nrows to set up the layout. Thus, we need to recalculated ncol
    ncol <- ceiling(nplots / nrow)
  }

  # Can be useful if "EffectData" objects of different models/datasets are c() together
  # For patchwork, we could also use its plot_layout(byrow = FALSE) argument.
  if (!byrow) {
    # alternating indices, e.g., 1, 4, 2, 5, 3 with ncol = 2 and nplots 0 5
    alt <- c(t(matrix(seq_len(nrow * ncol), ncol = ncol)))[J]
    x <- x[alt]
  }

  # col and row per subplot
  col_i <- (J - 1L) %% ncol + 1L
  row_i <- rep(seq_len(nrow), each = ncol)[J]

  # Should we hide right ticks (when scale is identical)
  hide_some_yticks <- share_y %in% c("all", "rows") || !is.null(ylim)

  # Shared y is solved via ylim + padding
  if (share_y != "no" && is.null(ylim)) {
    if (share_y == "all") {
      ylim <- common_range(x, stat_info = stat_info)
    } else {
      ix <- switch(share_y, rows = row_i, cols = col_i)
      ylim <- lapply(split(x, ix), common_range, stat_info = stat_info)[ix]
    }
  }
  if (!is.list(ylim)) {
    ylim <- replicate(nplots, ylim, simplify = FALSE)
  }

  if (show_legend) {
    # We prefer a numeric subplot as reference. Placement is done at the right center.
    show_legend <- seq_along(x) == order(!.num(x))[1L]
  }

  titles <- if (subplot_titles) names(x) else ""

  if (!plotly) {
    plot_list <- mapply(
      one_ggplot,
      x,
      v = names(x),
      title = titles,
      ylim = ylim,
      show_legend = show_legend,
      wrap_x = wrap_x,
      rotate_x = rotate_x,
      MoreArgs = list(
        share_y = share_y,
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
      patchwork::plot_layout(
        ncol = ncol,
        guides = "collect",
        axis_titles  = "collect",
        axes = if (hide_some_yticks) "collect" else "collect_x",
        ...
      )
    if (title != "") {
      p <- p + patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      )
    }
    p
  } else {
    plot_list <- mapply(
      one_plotly,
      x,
      v = names(x),
      title = titles,
      ylim = ylim,
      show_yticks = (col_i == 1L) | !hide_some_yticks,
      show_legend = show_legend,
      overlay = paste0("y", 2 * seq_along(x)),
      MoreArgs = list(
        title_as_ann = TRUE,
        share_y = share_y,
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
    margins <- c(0.05 - hide_some_yticks * 0.02, 0.05 + subplot_titles * 0.02)
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
        fig, margin = list(t = 40 + 10 * subplot_titles), title = title
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

one_ggplot <- function(
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
    f <- if (share_y != "no") -0.05 else -0.02
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

  # Add zero line if average residuals are to be shown
  if ("resid_mean" %in% stat_info) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.6)
  }

  # Add optional points
  if (!num || num_points) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = varying_),
      size = 2,
      alpha = alpha,
      show.legend = show_legend
    )
  }

  # Add optional lines
  if (num || cat_lines) {
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

one_plotly <- function(
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

  if (num && !num_points) {
    scatter_mode <- "lines"
  } else if (!num && !cat_lines) {
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
    f <- if (share_y == "no" && !is.null(ylim)) 1.01 else 0.98
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
      zeroline = ("resid_mean" %in% stat_info)
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

#' Plots Marginal Object
#'
#' Plots marginal statistics using a color blind palette from "ggthemes".
#' You can switch to plotly by setting `backend = "plotly"`.
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "marginal".
#' @param ncol Number of columns in the plot layout. Only for multiple plots.
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
#'   a reasonable name based on the calculated statistics.
#' @param lines Named vector of the form `c(name = statistic, ...)` of statistics to
#'   plot. `name` is used as legend label. By default
#'   `c(obs = "y_mean", pred = "pred_mean", pd = "pd")`.
#'   To show only bias with "bias" as legend entry, use `c(bias = "resid_mean")`.
#' @param colors Vector of line colors of sufficient length.
#'   By default, a color blind friendly palette from "ggthemes".
#'   To change globally, set `options(marginalplot.colors = new colors)`.
#' @param fill Fill color of bars. The default equals "lightgrey".
#'   To change globally, set `options(marginalplot.fill = new color)`.
#' @param bar_height Relative bar height (default 1). Set to 0 for no bars.
#' @param bar_width Relative bar width of non-numeric features, by default 0.7.
#' @param bar_measure What should bars represent? Either "weight" (default) or "N".
#' @param wrap_x Should categorical x axis labels be wrapped after this length?
#'   The default is 10. Set to 0 for no wrapping. Vectorized over `x`.
#'   Only for "ggplot2" backend.
#' @param rotate_x Should categorical xaxis labels be rotated by this angle?
#'   The default is 0 (no rotation). Vectorized over `x`. Only for "ggplot2" backend.
#' @param backend Plot backend, either "ggplot2" (default) or "plotly".
#'   To change globally, set `options(marginalplot.backend = "plotly")`.
#' @param ... Passed to `patchwork::plot_layout()` or `plotly::subplot()`. Typically
#'   not used.
#' @returns
#'   If single plot, an object of class  "ggplot" or "plotly".
#'   Otherwise, an object of class "patchwork", or a "plotly" subplot.
#' @seealso [marginal()], [average_observed()], [partial_dependence()]
#' @export
plot.marginal <- function(
    x,
    ncol = 2L,
    byrow = TRUE,
    share_y = FALSE,
    ylim = NULL,
    cat_lines = TRUE,
    num_points = FALSE,
    title = "",
    subplot_titles = TRUE,
    ylab = NULL,
    lines = c(obs = "y_mean", pred = "pred_mean", pd = "pd"),
    colors = getOption("marginalplot.colors"),
    fill = getOption("marginalplot.fill"),
    bar_height = 1,
    bar_width = 0.7,
    bar_measure = c("weight", "N"),
    wrap_x = 10,
    rotate_x = 0,
    backend = getOption("marginalplot.backend"),
    ...
) {
  bar_measure <- match.arg(bar_measure)

  stopifnot(
    backend %in% c("ggplot2", "plotly"),
    lines %in% c("y_mean", "pred_mean", "resid_mean", "pd")
  )

  nplots <- length(x)

  lines <- lines[lines %in% colnames(x[[1L]])]
  nn <- length(lines)
  show_legend <- nn > 1L

  stopifnot(
    nn > 0L,
    length(colors) >= nn
  )

  # Overwrite bin_width of categorical features
  x <- lapply(x, function(z) {if (!.num(z)) z$bin_width <- bar_width; z})

  # Derive a good ylab
  if (is.null(ylab)) {
    ylab <- get_ylab(lines)
  }

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
        lines = lines,
        colors = colors,
        fill = fill,
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
        lines = lines,
        colors = colors,
        fill = fill,
        bar_height = bar_height,
        bar_measure = bar_measure,
        show_legend = show_legend
      )
    }
    return(p)
  }

  # Now with multiple plots...
  if (backend == "plotly") {
    # Let's try to figure out how many columns our plotly subplot will really have...
    nrows <- ceiling(nplots / min(ncol, nplots))
    ncol <- ceiling(nplots / nrows)
  }

  # Can be useful if objects of different models/datasets are c() together
  if (isFALSE(byrow)) {
    alt <- c(t(matrix(seq_along(x), ncol = ncol)))
    x <- x[alt]
  }

  # Shared y is solved via ylim + padding
  if (share_y && is.null(ylim)) {
    r <- range(sapply(x, function(z) range(z[lines], na.rm = TRUE)))
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
        lines = lines,
        colors = colors,
        fill = fill,
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
    col_i <- (seq_along(x) - 1L) %% ncol + 1L
    plot_list <- mapply(
      plot_marginal_plotly,
      x,
      v = names(x),
      title = titles,
      show_yticks = !share_y | (col_i == 1L),
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
        lines = lines,
        colors = colors,
        fill = fill,
        bar_height = bar_height,
        bar_measure = bar_measure
      ),
      SIMPLIFY = FALSE
    )
    # left/right - top/bottom (we use it symmetrically)
    margins <- c(0.05 - isTRUE(share_y) * 0.02, 0.05 + isTRUE(subplot_titles) * 0.02)
    fig <- plotly::subplot(
      plot_list,
      titleX = TRUE,
      titleY = FALSE,
      nrows = nrows,
      widths = corr_margin(ncol, margin = margins[1L]),
      heights = corr_margin(nrows, margin = margins[2L]),
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
    lines,
    colors,
    fill,
    bar_height,
    bar_measure,
    wrap_x,
    rotate_x,
    show_legend = TRUE
) {
  num <- .num(x)
  df <- transform(
    poor_man_stack(x, lines),
    varying_ = factor(varying_, levels = lines, labels = names(lines))
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
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "right") +
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
    lines,
    colors,
    fill,
    bar_height,
    bar_measure,
    show_ylab = TRUE,    # not vectorized
    show_yticks = TRUE,  # vectorized, and all below as well
    show_legend = TRUE,
    overlay = "y2"
) {
  num <- .num(x)

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

  for (i in seq_along(lines)) {
    z <- lines[i]
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
      color = I(colors[i])
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

# Helper functions

# subplots make inner plots smaller due to margins
# https://github.com/plotly/plotly.R/issues/2144
# We apply an approximate correction factor via heights and widths
# The function assumes symmetric margin pairs ((left, right), (top, bottom))
corr_margin <- function(m, margin) {
  if (m >= 3L) {
    f <- 1 / m * (1 - 2 * margin)  # ok? Or rather 1 / m / (1 + 2 * margin)?
    n_inner <- m - 2L
    return(c(f, rep((1 - 2 * f) / n_inner, times = n_inner), f))
  }
  return(NULL)
}

get_ylab <- function(lines) {
  if (length(lines) == 1L) {
    out <- switch(
      lines,
      y_mean = "Average response",
      pred_mean = "Average prediction",
      resid_mean = "Bias",
      pd = "Partial Dependence"
    )
    return(out)
  }
  # No "average" (that would be overly specific)
  if ("y_mean" %in% lines) "Response" else "Prediction"
}

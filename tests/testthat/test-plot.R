fit <- lm(Sepal.Length ~ ., data = iris)
xvars <- colnames(iris)[-1]
M <- marginal(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)

test_that("plot() returns correct class", {
  expect_s3_class(plot(M), "patchwork")
  expect_s3_class(plot(M[1L]), "ggplot")

  # Plotly
  p <- plot(M, backend = "plotly")
  expect_s3_class(p, "plotly")
  expect_true("subplot" %in% names(p$x))

  p <- plot(M[1L], backend = "plotly")
  expect_s3_class(p, "plotly")
  expect_false("subplot" %in% names(p$x))
})

test_that("ncols has an effect", {
  # How to do with patchwork??


  # Plotly
  p <- list(
    p1 = plot(M, backend = "plotly", ncols = 1),
    p2 = plot(M, backend = "plotly", ncols = 4)
  )
  r <- lapply(p, function(z) diff(z$x$layout$xaxis$domain))
  expect_equal(r$p1, 1)
  expect_true(r$p2 <= 0.25)
})

test_that("ylim can bet set", {
  ylims <- c(2, 10)

  # One x
  out <- ggplot2::layer_scales(plot(M[1], ylim = ylims))$y$limits
  expect_equal(out, ylims)

  # Multiple x
  out <- ggplot2::layer_scales(plot(M, ylim = ylims))$y$limits
  expect_equal(out, ylims)

  # Plotly

  # One x
  p <- plotly::plotly_build(plot(M[1], backend = "plotly", ylim = ylims))
  out <- p$x$layout$yaxis$range
  expect_equal(out, ylims)

  # Multiple x
  p <- plot(M, backend = "plotly", ylim = ylims)
  out <- p$x$layout$yaxis$range
  expect_equal(out, ylims)
})

test_that("y axis can be shared", {
  # Test works because the first plot does not have the widest y range
  r1 <- diff(ggplot2::layer_scales(plot(M, share_y = FALSE))$y$range$range)
  r2 <- diff(ggplot2::layer_scales(plot(M, share_y = TRUE))$y$range$range)
  expect_true(r1 < r2)

  # Plotly
  p1 <- plot(M, backend = "plotly", share_y = FALSE)
  p2 <- plot(M, backend = "plotly", share_y = TRUE)

  expect_null(p1$x$layout$yaxis$range)
  expect_equal(p2$x$layout$yaxis$range, p2$x$layout$yaxis3$range)
})


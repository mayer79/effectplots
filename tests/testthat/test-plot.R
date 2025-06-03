fit <- lm(Sepal.Length ~ ., data = iris)
xvars <- colnames(iris)[-1]
M <- feature_effects(fit, v = xvars, data = iris, y = "Sepal.Length", breaks = 5)

test_that("plot() returns correct class", {
  expect_s3_class(plot(M, rotate_x = 45, title = "multiple plots"), "patchwork")
  expect_s3_class(plot(M, stats = "resid_mean", interval = "ci"), "patchwork")

  expect_s3_class(plot(M[1L], rotate_x = 45), "ggplot")
  expect_s3_class(plot(M[1L], stats = "resid_mean", interval = "ci"), "ggplot")

  # Plotly
  p <- plot(M, plotly = TRUE, title = "multiple plots")
  expect_s3_class(p, "plotly")
  expect_true("subplot" %in% names(p$x))

  p <- plot(M[1L], plotly = TRUE)
  expect_s3_class(p, "plotly")
  expect_false("subplot" %in% names(p$x))

  p <- plot(M, stats = "resid_mean", interval = "ci", plotly = TRUE)
  expect_s3_class(p, "plotly")
  expect_true("subplot" %in% names(p$x))

  p <- plot(M[1L], stats = "resid_mean", interval = "ci", plotly = TRUE)
  expect_s3_class(p, "plotly")
  expect_false("subplot" %in% names(p$x))
})

test_that("plot() returns correct class with single ALE line", {
  expect_s3_class(plot(M[1:2], stats = "ale"), "patchwork")
  expect_s3_class(plot(M[1L], stats = "ale"), "ggplot")

  # Plotly
  p <- plot(M[1:2], plotly = TRUE, stats = "ale")
  expect_s3_class(p, "plotly")
  expect_true("subplot" %in% names(p$x))

  p <- plot(M[1L], plotly = TRUE, stats = "ale")
  expect_s3_class(p, "plotly")
  expect_false("subplot" %in% names(p$x))
})


test_that("ncols has an effect", {
  # How to do with patchwork??


  # Plotly
  p <- list(
    p1 = plot(M, plotly = TRUE, ncol = 1),
    p2 = plot(M, plotly = TRUE, ncol = 4)
  )
  r <- lapply(p, function(z) diff(z$x$layout$xaxis$domain))
  expect_equal(r$p1, 1)
  expect_true(r$p2 <= 0.25)
})

test_that("ylim can bet set", {
  ylims <- c(2, 10)

  # Plotly

  # One x
  p <- plotly::plotly_build(plot(M[1], plotly = TRUE, ylim = ylims))
  out <- p$x$layout$yaxis$range
  expect_equal(out, ylims)

  # Multiple x
  p <- plot(M, plotly = TRUE, ylim = ylims)
  out <- p$x$layout$yaxis$range
  expect_equal(out, ylims)
})

test_that("y axis can be shared", {
  # Test works because the first plot does not have the widest y range
  r1 <- diff(ggplot2::layer_scales(plot(M, share_y = "no"))$y$range$range)
  r2 <- diff(ggplot2::layer_scales(plot(M, share_y = "all"))$y$range$range)
  expect_true(r1 < r2)

  # Plotly
  p1 <- plot(M, plotly = TRUE, share_y = "no")
  p2 <- plot(M, plotly = TRUE, share_y = "all")

  expect_null(p1$x$layout$yaxis$range)
  expect_equal(p2$x$layout$yaxis$range, p2$x$layout$yaxis3$range)
})

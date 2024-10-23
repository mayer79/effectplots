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


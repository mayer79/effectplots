test_that("ale() is consistent with feature_effects()", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v <- c("Sepal.Width", "Species")
  suppressMessages(
    ale <- ale(fit, v = v, data = iris, w = 1:150)
  )
  suppressMessages(
    marg <- feature_effects(
      fit,
      v = v, data = iris, calc_pred = FALSE, pd_n = 0, w = 1:150
    )
  )
  expect_equal(ale, marg)
})

test_that(".ale() respects case weights", {
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  br <- c(2, 2.5, 3, 3.5, 4.5)
  w <- c(rep(1L, times = 100L), rep(2L, times = 50L))
  ix <- rep(1:nrow(iris), times = w)

  res_w <- .ale(fit, v = v, data = iris, breaks = br, w = w)
  res_uw <- .ale(fit, v = v, data = iris[ix, ], breaks = br)

  expect_equal(res_w, res_uw)
})

test_that("ale() respects case weights", {
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  br <- c(2, 2.5, 3, 3.5, 4.5)
  w <- c(rep(1L, times = 100L), rep(2L, times = 50L))
  ix <- rep(1:nrow(iris), times = w)

  res_w <- ale(fit, v = v, data = iris, breaks = br, w = w)[[1L]]
  res_uw <- ale(fit, v = v, data = iris[ix, ], breaks = br)[[1L]]

  expect_equal(res_w[-4L], res_uw[-4L])
})

test_that("the level order of g does not matter in .ale()", {
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  br <- c(2, 2.5, 3, 3.5, 4.5)
  g1 <- factor(cut(iris[[v]], breaks = br, include.lowest = TRUE, labels = FALSE))
  g2 <- factor(g1, levels = rev(levels(g1)))
  res_1 <- .ale(fit, v = v, data = iris, breaks = br, g = g1)
  res_2 <- .ale(fit, v = v, data = iris, breaks = br, g = g2)

  expect_equal(res_1, res_2)
})

test_that(".ale() is consistent with uncentered ALEPlot v 1.1", {
  # We use debugonce(ALEPlot) to see the uncentered values
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  K <- 5
  br <- unique(stats::quantile(iris[[v]], probs = seq(0, 1, length.out = K + 1)))

  # debugonce(ALEPlot)
  # ALEPlot(
  #   iris,
  #   X.model = fit,
  #   pred.fun = function(X.model, newdata) as.numeric(predict(X.model, newdata)),
  #   J = v,
  #   K = 5
  # )
  reference <- c(0.0000000, 0.6103576, 0.8673605, 0.9488453, 1.1848637, 1.9006788)
  result <- .ale(fit, v = v, data = iris, breaks = br)

  expect_equal(result, reference, tolerance = 1e-6)
})

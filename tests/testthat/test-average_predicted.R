fit <- lm(Sepal.Length ~ ., data = iris)
v <- c("Sepal.Width", "Species")

test_that("average_predicted() is consistent with feature_effects()", {
  avg_pred <- average_predicted(iris[v], pred = predict(fit, iris))
  marg <- feature_effects(fit, v = v, data = iris, pd_n = 0, ale_n = 0)
  expect_equal(avg_pred, marg)
})

test_that("single vector input works", {
  pred <- predict(fit, iris)
  out1 <- average_predicted(iris$Species, pred = pred)
  names(out1) <- "Species"
  out2 <- average_predicted(iris$Species, pred = pred, x_name = "Species")
  out3 <- average_predicted(iris["Species"], pred = pred)
  expect_equal(out1, out2)
  expect_equal(out1, out3)
})

test_that("case weights are respected", {
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  br <- c(2, 2.5, 3, 3.5, 4.5)
  w <- c(rep(1L, times = 100L), rep(2L, times = 50L))
  ix <- rep(1:nrow(iris), times = w)

  res_w <- average_predicted(
    iris[v],
    pred = predict(fit, iris),
    breaks = br,
    w = w
  )[[1L]]
  res_uw <- average_predicted(
    iris[ix, v],
    pred = predict(fit, iris[ix, ]),
    breaks = br
  )[[1L]]

  expect_equal(res_w[-4L], res_uw[-4L])
})

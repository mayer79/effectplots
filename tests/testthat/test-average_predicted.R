fit <- lm(Sepal.Length ~ ., data = iris)
v <- c("Sepal.Width", "Species")

test_that("average_predicted() is consistent with marginal()", {
  avg_pred <- average_predicted(iris[v], pred = predict(fit, iris))
  marg <- marginal(fit, v = v, data = iris, pd_n = 0, ale_bin_size = 0)
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

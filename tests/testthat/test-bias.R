fit <- lm(Sepal.Length ~ ., data = iris)
v <- c("Sepal.Width", "Species")

test_that("bias() is consistent with feature_effects()", {
  b <- bias(iris[v], resid = iris$Sepal.Length - predict(fit, iris))
  marg <- feature_effects(
    fit, v = v, data = iris, y = iris$Sepal.Length, pd_n = 0, ale_n = 0
  )
  for (z in v) {
    expect_equal(b[[z]], marg[[z]][, names(marg[[z]]) %in% colnames(b[[z]])])
  }
})

test_that("single vector input works", {
  r <- rep(0, nrow(iris))
  out1 <- bias(iris$Species, resid = r)
  names(out1) <- "Species"
  out2 <- bias(iris$Species, resid = r, x_name = "Species")
  out3 <- bias(iris["Species"], resid = r)
  expect_equal(out1, out2)
  expect_equal(out1, out3)
})

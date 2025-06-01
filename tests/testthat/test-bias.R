fit <- lm(Sepal.Length ~ ., data = iris)
v <- c("Sepal.Width", "Species")

test_that("bias() is consistent with feature_effects()", {
  b <- bias(iris[v], resid = iris$Sepal.Length - predict(fit, iris))
  marg <- feature_effects(
    fit,
    v = v, data = iris, y = iris$Sepal.Length, pd_n = 0, ale_n = 0
  )
  for (z in v) {
    out <- b[[z]]
    attr(out, "discrete") <- NULL
    expect_equal(out, marg[[z]][names(marg[[z]]) %in% colnames(b[[z]])])
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

test_that("case weights are respected", {
  fit <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  v <- "Sepal.Width"
  br <- c(2, 2.5, 3, 3.5, 4.5)
  w <- c(rep(1L, times = 100L), rep(2L, times = 50L))
  ix <- rep(1:nrow(iris), times = w)
  r <- iris$Sepal.Length - predict(fit, iris)

  res_w <- bias(
    iris[v],
    resid = r,
    breaks = br,
    w = w
  )[[1L]]
  res_uw <- bias(
    iris[ix, v],
    resid = r[ix],
    breaks = br
  )[[1L]]

  expect_equal(res_w[-4L], res_uw[-4L])
})

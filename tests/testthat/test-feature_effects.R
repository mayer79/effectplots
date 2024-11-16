test_that("setting a seed works", {
  X <- data.frame(matrix(runif(1e5), ncol = 4))
  pf <- function(m, x) rowMeans(x)

  M1 <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf, seed = 7)
  M2 <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf, seed = 7)
  expect_equal(M1, M2)
})

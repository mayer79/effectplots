test_that("setting a seed works", {
  X <- data.frame(matrix(runif(1e5), ncol = 4))
  pf <- function(m, x) rowMeans(x)

  M1 <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf, seed = 7)
  M2 <- feature_effects("anymodel", v = colnames(X), data = X, pred_fun = pf, seed = 7)
  expect_equal(M1, M2)
})

test_that("discrete output keeps its type", {
  X <- transform(
    iris,
    char = rep(c("A", "B", "C"), times = 50),
    logical = Sepal.Width > median(Sepal.Width),
    int = cut(iris$Petal.Width, breaks = 0:3, labels = FALSE)
  )
  fit <- lm(Sepal.Length ~ Species + Petal.Length + char + logical + int, data = X)
  M <- feature_effects(
    fit, v = c("Species", "Petal.Length", "char", "logical", "int"), data = X
  )
  # M$Species[3, c("bin_mid", "bin_mean")] <- NA
  # M$char[3, c("bin_mid", "bin_mean")] <- NA
  # M$logical[2, c("bin_mid", "bin_mean")] <- NA
  expect_equal(is_discrete(M), c(TRUE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(
    vapply(M, function(z) typeof(z$bin_mid), FUN.VALUE = character(1), USE.NAMES = F),
    c("integer", "double", "character", "logical", "integer")
  )

  expect_no_error(plot(M))
  expect_no_error(plot(M, plotly = TRUE))
})

test_that("constant columns work", {
  n <- 100
  X <- data.frame(
    double = 1,
    int = 1L,
    logical = TRUE,
    char = "A",
    factor = factor("A"),
    na = NA_real_
  )[rep(1, n), ]

  M <- feature_effects(
    object = NULL,
    v = colnames(X),
    data = X,
    pred_fun = function(m, x) rep(1, nrow(x)),
    y = 1:n
  )

  xp_stats <- data.frame(
    N = n,
    weight = n,
    pred_mean = 1,
    y_mean = mean(1:n),
    resid_mean = mean(1:n) - 1,
    y_sd = sd(1:n),
    resid_sd = sd(1:n - 1),
    pd = 1,
    ale = NA_real_  # is discrete
  )

  for (v in colnames(X)) {
    xp_pos <- data.frame(bin_mid = X[1, v], bin_width = 0.7, bin_mean = X[1, v])
    if (v == "na") {
      # This is modified in calculate_stats() to treat that case non-numeric
      xp_pos$bin_mid <- xp_pos$bin_mean <- NA_character_
    }
    xp <- cbind(xp_pos, xp_stats)
    attr(xp, "discrete") <- TRUE
    expect_equal(M[[v]], xp)
  }

  expect_no_error(plot(M))
  expect_no_error(plot(M, plotly = TRUE))
})

test_that("breaks can be specified, and bins have correct width", {
  X <- data.frame(
    a = c(NA, 1:10),
    b = c(rep("A", 10), NA),
    c = c(1:11)
  )

  M <- feature_effects(
    object = NULL,
    v = colnames(X),
    data = X,
    pred_fun = function(m, x) rep(1, nrow(x)),
    breaks = list(a = c(0, 11), c = c(2, 3, 10)),
    discrete_m = 2L
  )

  expect_equal(sapply(M, nrow), c(a = 2, b = 2, c = 2))
  expect_equal(M$a$bin_width, c(11, 11 / 2))
  expect_equal(M$b$bin_width, c(0.7, 0.7))
  expect_equal(M$c$bin_width, c(1, 7))

  expect_no_error(plot(M))
  expect_no_error(plot(M, plotly = TRUE))
})

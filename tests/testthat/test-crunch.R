test_that("factor_or_double() works", {
  # Character
  x <- c("B", NA, "A")
  expect_equal(factor_or_double(x), collapse::qF(x, sort = FALSE, na.exclude = FALSE))

  # Factor
  x <- factor(x, levels = c("A", "B", "C"))
  expect_equal(
    factor_or_double(x), collapse::qF(x, sort = TRUE, na.exclude = FALSE, drop = TRUE)
  )

  # Logical
  x <- c(TRUE, FALSE)
  expect_equal(factor_or_double(x), collapse::qF(x, sort = FALSE, na.exclude = FALSE))

  # Non-discrete numeric vector (short)
  x <- 1:10
  res <- factor_or_double(x, m = 4)
  expect_equal(res, x)
  expect_true(is.double(res))

  # Discrete numeric vector (short)
  x <- rep(1:4, each = 10)
  res <- factor_or_double(x, m = 4)
  expect_equal(res, collapse::qF(x, na.exclude = FALSE, sort = FALSE))

  # Non-discrete numeric vector (long)
  x <- 1:10
  res <- factor_or_double(x, m = 4, ix_sub = 1:5)
  expect_equal(res, x)

  # Discrete numeric vector (long)
  x <- rep(1:4, each = 10)
  res <- factor_or_double(x, m = 4, ix_sub = 1:5)
  expect_equal(res, collapse::qF(x, na.exclude = FALSE, sort = FALSE))

  # m needs to be smaller than length(ix_sub)
  expect_error(factor_or_double(x, m = 4, ix_sub = 1:2))
})

test_that("grouped_stats() works", {
  x <- cbind(a = 1:6, b = 6:1)
  g <- c(2, 2, 1, 1, 1, 1)
  w1 <- rep(2, times = 6)
  w2 <- 1:6

  r <- grouped_stats(x, g = g)
  rownames(r) <- NULL
  expect_equal(
    r[, 1:4],
    cbind(N = c(4, 2), weight = c(4, 2), a_mean = c(4.5, 1.5), b_mean = c(2.5, 5.5))
  )

  # Grouped and weighted
  rw1 <- grouped_stats(x, g = g, w = w1)
  rownames(rw1) <- NULL
  expect_equal(r[, c(1, 3:4)], rw1[, c(1, 3:4)])

  rw2 <- grouped_stats(x, g = g, w = w2)
  g1 <- colSums(x[g == 1, ] * w2[g == 1]) / sum(w2[g == 1])
  g2 <- colSums(x[g == 2, ] * w2[g == 2]) / sum(w2[g == 2])
  expect_equal(unname(rw2[, 1:4]), unname(cbind(c(4, 2), c(18, 3), rbind(g1, g2))))
})

test_that("Test that grouped_stats() uses sort(funique) + NA as order", {
  f1 <- c("b", "c", "c", NA, "a", "b")
  ff <- list(
    fact = factor(f1, levels = c("c", "b", "a")),
    float = c(3, 3, 1, 2, NA, 2),
    int = c(3L, 3L, 1L, 2L, NA, 2L),
    logi = c(TRUE, FALSE, FALSE, FALSE, NA, TRUE),
    char = f1
  )
  for (f in ff) {
    out <- rownames(grouped_stats(cbind(s = 1:6), g = f))
    expect_equal(out, as.character(sort(collapse::funique(f), na.last = TRUE)))
  }
})

test_that("hist2() typically gives identical breaks than graphics::hist()", {
  set.seed(1)
  x <- rnorm(1000)

  breaks <- list(5, -10:10, "sturges")
  for (b in breaks) {
    expect_equal(hist2(x, b), graphics::hist(x, b, plot = FALSE)$breaks)
  }
})

test_that("hist2() does not like unknown strings", {
  expect_error(hist2(1:10, breaks = "scott"))
})

test_that("findInterval_equi() provides equal results as findInterval()", {
  x <- c(-1, NA, 2, 1, 0.5, 0, 10)
  br <- seq(0, 2, length.out = 5 + 1)
  for (r in c(FALSE, TRUE)) {
    expect_equal(
      findInterval_equi(x, low = 0, high = 2, nbin = 5L, right = r),
      findInterval(x, br, rightmost.closed = TRUE, all.inside = TRUE, left.open = r)
    )
  }
})

test_that("findInterval2() provides equal results as findInterval()", {
  x <- c(-1, NA, 2, 1, 1.2, 1.1999, 0.5, 0, 10)
  br <- seq(0, 2, length.out = 6)
  for (r in c(FALSE, TRUE)) {
    expect_equal(
      effectplots:::findInterval2(x, br, right = r),
      findInterval(x, br, rightmost.closed = TRUE, all.inside = TRUE, left.open = r)
    )
  }

  # Special cases
  expect_equal(findInterval2(x, 1:2, right = r), rep(1, length = length(x)))
  expect_equal(findInterval2(x, 1, right = r), rep(1, length = length(x)))

  # Breaks increasing?
  expect_error(findInterval2(x, breaks = c(4, 3, 1)))
})


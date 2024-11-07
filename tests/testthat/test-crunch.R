test_that("more_than_m_distinct() works for different ranges of x", {
  expect_false(more_than_m_distinct(1:10, m = 10))
  expect_true(more_than_m_distinct(1:10, m = 9))

  expect_true(more_than_m_distinct(1:1e5, m = 10))
  expect_false(more_than_m_distinct(rep(1, times = 1e5), m = 1))
})

test_that("winsorize() works", {
  x <- 1:10
  expect_equal(winsorize(x), x)
  expect_equal(winsorize(x, low = 2), pmax(2, x))
  expect_equal(winsorize(x, high = 8), pmin(8, x))  # not symmetric
  expect_equal(winsorize(x, low = 3, high = 7), pmax(3, pmin(7, x)))
})

test_that("poor_man_stack() works (test could be improved)", {
  y <- c("a", "b", "c")
  z <- c("aa", "bb", "cc")
  X <- data.frame(x = 1:3, y = y, z = z)
  out <- poor_man_stack(X, to_stack = c("y", "z"))
  xpected <- data.frame(
    x = rep(1:3, times = 2L),
    varying_ = factor(rep(c("y", "z"), each = 3L)),
    value_ = c(y, z)
  )
  expect_equal(out, xpected)
  expect_error(poor_man_stack(cbind(a = 1:3, b = 2:4), to_stack = "b"))
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

  breaks <- list(
    5,
    -10:10,
    "FD",
    "sturges",
    "scott",
    function(x) sqrt(length(x))
  )
  for (b in breaks) {
    expect_equal(hist2(x, b), graphics::hist(x, b, plot = FALSE)$breaks)
  }
})

test_that("hist2() works slightly different for very large vectors", {
  x <- 1:1e6
  expect_equal(hist2(x, breaks = "scott"), hist(x, breaks = 50, plot = FALSE)$breaks)
})

test_that("hist2() does not like unknown strings", {
  expect_error(hist2(1:10, breaks = "hello"))
})

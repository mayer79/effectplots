test_that("parse_rownames() does what it should", {
  x <- c("3", "1", NA)
  lev <- c("3", "1")
  expect_equal(parse_rownames(x, "character"), x)
  expect_equal(parse_rownames(x, "integer"), c(3L, 1L, NA))
  expect_equal(parse_rownames(x, "factor", lev = lev), factor(x, levels = lev))
  expect_equal(parse_rownames(c("3.11111", "2", NA), "double"), c(3.11111, 2, NA))
  expect_equal(parse_rownames(c("TRUE", NA), "logical"), c(TRUE, NA))

  xord <- c("A", "B", NA)
  expect_equal(parse_rownames(xord, "factor", ord = TRUE, lev = c("A", "B")), ordered(xord))

  expect_error(parse_rownames(x, "date"))
  expect_error(parse_rownames(1:3, "integer"))
  expect_error(parse_rownames(x, "factor"))
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


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


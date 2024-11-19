test_that("parse_rownames() does what it should", {
  x <- c("3", "1", NA)
  expect_equal(parse_rownames(x, "character"), x)
  expect_equal(parse_rownames(x, "integer"), c(3L, 1L, NA))
  expect_equal(parse_rownames(x, "factor"), factor(c("3", "1", NA), levels = c("3", "1")))
  expect_equal(parse_rownames(c("3.11111", "2", NA), "double"), c(3.11111, 2, NA))
  expect_equal(parse_rownames(c("TRUE", NA), "logical"), c(TRUE, NA))

  expect_error(parse_rownames(x, "date"))
  expect_error(parse_rownames(1:3, "integer"))
})



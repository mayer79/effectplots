test_that("hist2() gives identical breaks than graphics::hist()", {
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

test_that("hist2() does not like unknown strings", {
  expect_error(hist2(1:10, breaks = "hello"))
})


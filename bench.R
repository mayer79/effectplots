library(ranger)
library(ggplot2)

pf <- function(m, x) predict(m, x)$predictions
fit <- ranger(price~carat + color + cut + clarity, data = diamonds)

pred <- predict(fit, diamonds)$predictions
bench::mark(M <- marginal.default(
    fit,
    x_name = "carat",
    pred = pred,
    data = diamonds,
    y = "price",
    pred_fun = pf,
    pd_n = 500
  )
)
bench::mark(predict(fit, diamonds)$predictions)
bench::mark(predict(fit, iris))

bench::mark(partial_dep(fit, "carat", diamonds, grid = M$data$eval_at, pred_fun = pf))


plot.marginal(M)

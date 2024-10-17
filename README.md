# marginalplot <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/mayer79/marginalplot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/marginalplot/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

{marginalplot} provides high-quality plots for modelling. 

The workflow is as follows:

1. Crunch values via `marginal()` or the convenience wrappers `average_observed()` and `partial_dependence()`.
2. Post-process the results with `postprocess()`, e.g., to collapse rare levels of a categorical feature.
3. Plot the results with `plot()`. By default, the plots use {ggplot2} (single plots) and {patchwork} (multiple plots). Using `plot(backend = "plotly"), interactive plots are produced.

**Notes**

- The implementation is optimized for speed and convenience.
- Only models with numeric predictions (regression and binary classification) are supported.
- Most models (including DALEX explainers) work out-of-the box. If not, a tailored prediction function can be provided.

## Installation

You can install the development version of {marginalplot} from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mayer79/marginalplot")
```

## Usage

``` r
library(marginalplot)
library(ranger)

set.seed(1)

fit <- ranger(Sepal.Length ~ ., data = iris)
xvars <- c("Sepal.Width", "Petal.Width", "Petal.Length", "Species")

marginal(fit, x_name = xvars, data = iris, y = "Sepal.Length") |> 
  postprocess(na.rm = TRUE) |> 
  plot()
```

![](man/figures/marginal1.svg)

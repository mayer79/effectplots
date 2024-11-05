# effectplots <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/mayer79/effectplots/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/effectplots/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test coverage](https://codecov.io/gh/mayer79/effectplots/graph/badge.svg)](https://app.codecov.io/gh/mayer79/effectplots)
<!-- badges: end -->

**{effectplots}** is a fast R package for calculating and plotting effects of any model.

The main function `marginal()` calculates the following statistics per feature X over values/bins:

- **Average observed y values**: Used to assess descriptive associations between response y and features.
- **Average predictions** (M Plots, Apley [1]): Shows the combined effect of X and other (correlated) features. The difference to average observed y values measures model bias.
- **Average residuals:** Calculated when both `y` and predictions are available. Useful to study model bias.
- **Partial dependence** (Friedman [2]): How does the average prediction changes with X, keeping other feature values fixed?
- **Accumulated local effects** (Apley [1]): Alternative to partial dependence.

See the fantastic book of Christoph Molnar [3] for their background.

**Workflow**

1. Crunch values via `marginal()` or the convenience wrappers `average_observed()`, `average_predicted()`, `bias()`, `partial_dependence()`, and `ale()`.
2. Post-process the results with `update()`, e.g., to collapse rare levels of categorical features or to sort the results by a simple variable importance measure.
3. Plot the results with `plot()`.

**Some cool things**

- You can switch between {ggplot2}/{patchwork} plots and interactive {plotly} plots.
- The implementation is optimized for large data using {collapse}.
- Most models (including DALEX explainers and meta-learners such as Tidymodels) work out-of-the box. If not, a tailored prediction function can be specified.
- Binning of numeric features is done by the same options as `stats::hist()`. Additionally, outliers are capped (not removed) at +-2 IQR from the quartiles by default.
- As additional statistics, per-bin counts/weights are calculated, and also standard deviations of observed y, predictions, and residuals.
- Case weights are supported via the argument `w`.

## Installation

You can install the development version of {effectplots} from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mayer79/effectplots")
```

## Usage

We use synthetic data with 1 Mio rows containing information on Motor TPL insurance policies and claims.
The aim is to model claim frequency as a function of features like "driver_age" and "car_power".

Before modeling, we want to study association between features and response.

``` r
library(effectplots)
library(OpenML)
library(lightgbm)

set.seed(1)

df <- getOMLDataSet(data.id = 45106L)$data
# df <- arrow::read_parquet("claims.parquet")

xvars <- c("year", "town", "driver_age", "car_weight", "car_power", "car_age")

# 0.3s on laptop
average_observed(df[xvars], y = df$claim_nb) |>
  update(sort_by = "y_mean") |> 
  plot(share_y = "all")
```

![](man/figures/avg_obs.svg)

The plots have been automatically sorted by decreasing (weighted) variance of the average observed values. A shared y axis helps to compare the strength of the association across features.

### Fit model

Next, let's fit a boosted trees model.

```r
# Data splits
ix <- sample(nrow(df), 0.8 * nrow(df))
train <- df[ix, ]
test <- df[-ix, ]
X_train <- data.matrix(train[xvars])
X_test <- data.matrix(test[xvars])

# Training, using slightly optimized parameters found via cross-validation
params <- list(
  learning_rate = 0.05,
  objective = "poisson",
  num_leaves = 7,
  min_data_in_leaf = 50,
  min_sum_hessian_in_leaf = 0.001,
  colsample_bynode = 0.8,
  bagging_fraction = 0.8,
  lambda_l1 = 3,
  lambda_l2 = 5,
  num_threads = 7
)

fit <- lgb.train(
  params = params,
  data = lgb.Dataset(X_train, label = train$claim_nb),
  nrounds = 300
)
```

### Inspect model

After modeling, we use the test (or validation) data to crunch average observed, average predicted, partial dependence, and accumulated local effects per feature values/bins to gain insights about the model. Calculations are lightning fast.

```r
# 0.3s on laptop
marginal(fit, v = xvars, data = X_test, y = test$claim_nb) |>
  update(sort_by = "pd") |> 
  plot()
```

![](man/figures/marginal.svg)

**Comments**

1. Comparing average predictions with average observed y provides a good picture of model bias. In this case, the bias on the test data seems to be small. Studying the same plot on the training data would help to assess in-sample bias.
2. Comparing the shape of partial dependence or ALE with the shape of the average predicted curve provides additional insights. E.g., for the two strong predictors "driver_age" and "car_power", these lines are very similar. This means the marginal effects are mainly due to the feature on the x axis and not of some other, correlated, feature.
3. Sorting is done by decreasing weighted variance of the partial dependence values, a measure of main-effect strength closely related (but not 100% identical) to [4].

### Flexibility

Thanks to the flexibility of the package, you can modify the results as you wish. For instance: what about putting results on training data besides those on test? Or comparing different models or subgroups? 

```r
m_train <- marginal(fit, v = xvars, data = X_train, y = train$claim_nb)
m_test <- marginal(fit, v = xvars, data = X_test, y = test$claim_nb)

# Pick top 3 based on train
m_train <- m_train |> 
  update(sort_by = "pd") |> 
  head(3)
m_test <- m_test[names(m_train)]

# Plot combined one
c(m_train, m_test) |> 
  plot(
    share_y = "rows",
    ncol = 2,
    byrow = FALSE,
    statistics = c("y_mean", "pred_mean"),
    subplot_titles = FALSE,
    title = "Left: Train - Right: Test",
  )
```

![](man/figures/train_test.svg)

# References

1. Apley, Daniel W., and Jingyu Zhu. 2020. *Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models.* Journal of the Royal Statistical Society Series B: Statistical Methodology, 82 (4): 1059–1086. doi:10.1111/rssb.12377.
2. Friedman, Jerome H. 2001. *Greedy Function Approximation: A Gradient Boosting Machine.* Annals of Statistics 29 (5): 1189–1232. doi:10.1214/aos/1013203451.
3. Molnar, Christoph. 2019. *Interpretable Machine Learning: A Guide for
Making Black Box Models Explainable*. <https://christophm.github.io/interpretable-ml-book>.
4. Greenwell, Brandon M., Bradley C. Boehmke, and Andrew J. McCarthy. 2018.
*A Simple and Effective Model-Based Variable Importance Measure.* arXiv preprint. <https://arxiv.org/abs/1805.04755>.

# effectplots 0.2.2

### Minor improvement

- `update(, collapse_m = m)` will now produce a level "other p" for the p least frequent categories [#52](https://github.com/mayer79/effectplots/pull/52).
- `update()` has received new default: `collapse_m = 15` (was 30) [#54](https://github.com/mayer79/effectplots/pull/54).
- Missing values in `y` are now checked *after* removing observations without positive weights [#56](https://github.com/mayer79/effectplots/pull/56).
- Plots now skip missing values on the y-axis, also in the ribbons [#57](https://github.com/mayer79/effectplots/pull/57).
- Factors with explicit NA level are respected, [#59](https://github.com/mayer79/effectplots/pull/59).

### Minor changes

# effectplots 0.2.1

### Major improvement

- NA values on x axis are always plotted, even for numeric features [#49](https://github.com/mayer79/effectplots/pull/49).

### Minor changes

- ggplot plots with no y variation would not show the exposure bars. This has been fixed in [#49](https://github.com/mayer79/effectplots/pull/49).
- Added {labeling} and {scales} explicitly to list of dependencies. Both are required by {ggplot2} anyway [#49](https://github.com/mayer79/effectplots/pull/49). 

# effectplots 0.2.0

### Major bug fixes

- The outlier clipping algorithm has unintentionally modified the values in place, i.e., also in the original dataframe. This is fixed by [#24](https://github.com/mayer79/effectplots/pull/24).

### Efficiency improvements

- Significant speed-up and memory reduction for numeric features [#16](https://github.com/mayer79/effectplots/pull/16), [#24](https://github.com/mayer79/effectplots/pull/24), [#25](https://github.com/mayer79/effectplots/pull/25).
- The barebone ALE function `.ale()` has become faster thanks to [issue #11](https://github.com/mayer79/effectplots/issues/11) by [@SebKrantz](https://github.com/SebKrantz).
- Subsampling indices for outlier capping is now done only once, instead of once per feature [#15](https://github.com/mayer79/effectplots/pull/15).

### Minor bug fixes

- NA values in feature columns have not been counted in the counts "N".
- Ordered factors are now working properly.
- ALE are correct also with empty bins at the border (could happen with user-defined breaks).
- `update(collapse_m = ...)` has collapsed wrong categories [#31](https://github.com/mayer79/effectplots/pull/31), [#34](https://github.com/mayer79/effectplots/pull/34), and [#35](https://github.com/mayer79/effectplots/pull/36).

### Documentation

- README has received examples for Tidymodels and probabilistic classification.
- Updated function documentation [#41](https://github.com/mayer79/effectplots/pull/41).

### Other changes

- Plots with more than one line now use "Effect" als default y label.
- Automatic break count selection via "FD", "Scott" and via function is not possible anymore [#24](https://github.com/mayer79/effectplots/pull/24).
- Export of `fcut()`, a fast variant of `cut()` [#25](https://github.com/mayer79/effectplots/pull/25).
- x axes are not collected anymore by {patchwork} [#27](https://github.com/mayer79/effectplots/pull/27).
- The default of `discrete_m = 5` has been increased to 13 [#29](https://github.com/mayer79/effectplots/pull/29).
- Slightly different check/preparation of predictions (and the argument `pred`). Helps to simplify the use of {h2o} [#32](https://github.com/mayer79/effectplots/pull/32).
- Updated Plotly subplots layout [#33](https://github.com/mayer79/effectplots/pull/33), [#43](https://github.com/mayer79/effectplots/pull/43), [#44](https://github.com/mayer79/effectplots/pull/44), [#45](https://github.com/mayer79/effectplots/pull/45).
- Better test coverage, e.g., [#34](https://github.com/mayer79/effectplots/pull/34).
- (Slowish) support for h2o models [#36](https://github.com/mayer79/effectplots/pull/36).
- Row names of statistics of numeric features are now removed [#37](https://github.com/mayer79/effectplots/pull/37).
- ALE values are now plotted at the right bin break (instead of bin mean) [#38](https://github.com/mayer79/effectplots/pull/38).
- Empty factor levels in features are not anymore dropped. However, you can use `update(..., drop_empty = TRUE)` to drop them after calculations [#40](https://github.com/mayer79/effectplots/pull/40).
- Better input checks for `average_observed()`, `average_predicted()`, and `bias()` [#41](https://github.com/mayer79/effectplots/pull/41).
- `plot()`: Renamed argument `num_points` to `continuous_points` and `cat_lines` to `discrete_lines`  [#42](https://github.com/mayer79/effectplots/pull/42).
- `update()`: New argument `to_factor` to turn discrete non-factors to factors [#42](https://github.com/mayer79/effectplots/pull/42).
- EffectData class: Discrete feature values in the output class are represented by their original data types instead of converting them to factors [#42](https://github.com/mayer79/effectplots/pull/42).
- EffectData class: The data.frames in the output now contain an attributes `discrete` to distinguish continuous from discrete features [#42](https://github.com/mayer79/effectplots/pull/42).
- `effect_importance()` will produce an error when sorting on non-existent statistic [#45](https://github.com/mayer79/effectplots/pull/45).

# effectplots 0.1.0

Initial release.

# effectplots 0.1.1

### Major bug fixes

- The outlier clipping algorithm has unintentionally modified the values in place, i.e., also in the original dataframe. This is fixed by [#24](https://github.com/mayer79/effectplots/pull/24).

### Efficiency improvements

- Significant Speed-up and memory reduction for discrete numeric features [#16](https://github.com/mayer79/effectplots/pull/16).
- Significant speed-up and memory reduction for continuous features by delegating outlier clipping to the binning algorithm [#24](https://github.com/mayer79/effectplots/pull/24).
- The barebone ALE function `.ale()` has become faster thanks to [issue #11](https://github.com/mayer79/effectplots/issues/11) by [@SebKrantz](https://github.com/SebKrantz).
- Subsampling indices for outlier capping is now done only once, instead of once per feature [#15](https://github.com/mayer79/effectplots/pull/15).

### Minor bug fixes

- NA values in feature columns have not been counted in the counts "N".
- Ordered factors are now working.
- ALE are correct also with empty bins at the border (could happen with user-defined breaks).

### Documentation

- README has received examples for Tidymodels and probabilistic classification.

### Other changes

- Plots with more than one line now use "Effect" als default y label.
- Automatic break count selection via "FD", "Scott" and via function is not possible anymore ([#24](https://github.com/mayer79/effectplots/pull/24)).

# effectplots 0.1.0

Initial release.

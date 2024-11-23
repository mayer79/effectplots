# effectplots 0.1.1

### Efficiency improvements

- The barebone ALE function `.ale()` has become faster thanks to [issue #11](https://github.com/mayer79/effectplots/issues/11) by [@SebKrantz](https://github.com/SebKrantz).
- Subsampling indices for outlier capping is now done only once, instead of once per feature [#15](https://github.com/mayer79/effectplots/pull/15).
- Speed-up for discrete numeric features [#16](https://github.com/mayer79/effectplots/pull/16).

### Bug fixes

- NA values in feature columns have not been counted in the counts "N".
- Ordered factors are now working.

### Documentation

- README has received examples for Tidymodels and probabilistic classification.

### Other changes

- Plots with more than one line now use "Effect" als default y label.

# effectplots 0.1.0

Initial release.

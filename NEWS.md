# effectplots 0.1.1

## Efficiency improvements

- The barebone ALE function `.ale()` has become faster thanks to [issue #11](https://github.com/mayer79/effectplots/issues/11) by [@SebKrantz](https://github.com/SebKrantz).
- Subsampling indices for outlier capping is now done only once, instead of once per feature.
- Speed-up for discrete numeric features.

## Bug fixes

- NA values in feature columns have not been counted in the counts "N".

# effectplots 0.1.0

Initial release.

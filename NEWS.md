# flipr (development version)

# flipr 0.3.3

- Updated URLs in `DESCRIPTION` file;
- Properly documented package overview in `flipr-package.R` file;
- Updated GitHub Action workflows;
- Removed suggested dependency to the entire tidyverse;
- Updated maintainer email.

# flipr 0.3.2

- Removed `akima` from package dependencies.
- Updated GHA workflows and `roxygen2` version.
- Updated all URLs to reflect change of ownership.

# flipr 0.3.1

- Remove any occurrence of base-R pipe operator in favor of [**magrittr**](https://magrittr.tidyverse.org) pipe operator for now for backward compatibility with older versions of R.
- Improved way of handling statistics that rely on inter-point distances.
- Method `$get_value()` gains optional arguments `keep_null_distribution` and `keep_permutations` in case the user would like to keep track of either the permutation null distribution or the sampled permutations that have been used to produce it.
- Method `$get_value()` now handles extra parameters to be passed to the test statistics.

# flipr 0.3.0

## Breaking changes

The API has been changed:

* An [R6](https://r6.r-lib.org) class is now used to define the plausibility function, with methods that implement point estimation, confidence intervals and evaluation on grid for later plotting.
* The plotting capability is still outside the class but might move into the class methods as well in the future.

## Minor changes

* Test statistics based on Inter-point distances have been added and implemented in C++ through the [**Rcpp**](http://dirk.eddelbuettel.com/code/rcpp.html) package.
* Test coverage is set and achieves 20% coverage for now (#3, @C-Juliette).
* @C-Juliette has been added as contributor to the package.
* The p-value function has been consistently renamed plausibility function to better reflects what it stands for and avoid confusion with other definitions of the p-value function in the area of functional data analysis.

# flipr 0.2.1

* Fix warning when installing the package on Oracle Solaris 10, x86, 32 bit,
R-release due to pandoc version not available preventing the `pvalue-function` vignette from knitting properly.

# flipr 0.2.0

## New features

* Compute p-value surfaces when inferring multiple parameters at once via
`two_sample_pf()`;
* Compute a point estimation for a single parameter of interest via
`two_sample_pe()`;
* Compute a confidence interval for a single parameter of interest via
`two_sample_ci()`;
* Automatically draw the p-value function for a single parameter of interest via
`two_sample_viz()`.

## Full list of changes

* Change argument names `test` and `combining_function` to `type` and
`combine_with` respectively. Add argument `alternative` to specify which kind of
test is performed.
* Add a function to compute p-value functions for sets of null hypotheses.
* Draft of article illustrating the computation of p-value functions with
[**flipr**](https://permaverse.github.io/flipr/).
* Add $t$, mean and Fisher test statistics.
* Correct two-tail p-value computation.
* Better API for pvalue function.
* Added a function for two sample confidence interval for a single parameter.
* Added an article about permutation inference overview.
* Added dependency to the [**ggplot2**](https://ggplot2.tidyverse.org) package
for plotting features.
* Added dependency to the [**cli**](https://cli.r-lib.org) package for
displaying important information to the user in an elegant fashion.
* Added function for point estimation of a single parameter.
* Added function for confidence interval of a single parameter.
* Added function for automatic plotting of the p-value function of a single
parameter.
* Added dependency to the [**withr**](https://withr.r-lib.org) package for
properly handle local random seed generator settings.
* Added dependency to the
[**viridisLite**](https://github.com/sjmgarnier/viridisLite) package for using
colors compatible with most common types of color blindness.

# flipr 0.1.1

* Fix warning when installing the package on Oracle Solaris 10, x86, 32 bit,
R-release due to pandoc version not available.
* Switch to GPL-3 license because **purrr** dependency is GPL-3 licensed.

# flipr 0.1.0

* Initial release.
* Added a `NEWS.md` file to track changes to the package.

#' (M)ANOVA Permutation Test
#'
#' This function carries out an hypothesis test in which the null hypothesis is
#' that K samples are governed by the same underlying generative probability
#' distribution against the alternative hypothesis that they are governed by
#' different generative probability distributions.
#'
#' @section User-supplied statistic function:
#' A user-specified function should have at least two arguments:
#'
#' - the first argument should be either a list of the `n` pooled data points or
#' a dissimilarity matrix stored as a [`stats::dist`] object.
#' - the second argument shoud be an integer vector specifying the (possibly
#' permuted) membership of each data point.
#'
#' See the [`stat_anova_f()`] function for an example.
#'
#' @param data A numeric vector or a numeric matrix or a list specifying the
#'   pooled data points. Alternatively, it can be a distance matrix stored as an
#'   object of class [`stats::dist`], in which case test statistics
#'   based on inter-point distances (marked with the `_ip` suffix) should be
#'   used.
#' @param memberships An integer vector specifying the original membership of
#'   each data point.
#' @param stats A list of functions produced by [`rlang::as_function()`]
#'   specifying the chosen test statistic(s). A number of test statistic
#'   functions are implemented in the package and can be used as such.
#'   Alternatively, one can provide its own implementation of test statistics
#'   that (s)he deems relevant for the problem at hand. See the section
#'   *User-supplied statistic function* for more information on how these
#'   user-supplied functions should be structured for compatibility with the
#'   **flipr** framework. Defaults to `list(stat_anova_f_ip)`.
#' @param B The number of sampled permutations. Default is `1000L`.
#' @param M The total number of possible permutations. Defaults to `NULL`, which
#'   means that it is automatically computed from the given sample size(s).
#' @param alternative A single string or a character vector specifying whether
#'   the p-value is right-tailed, left-tailed or two-tailed. Choices are
#'   `"right_tail"`, `"left_tail"` and `"two_tail"`. Default is `"two_tail"`. If
#'   a single string is provided, it is assumed that it should be applied to all
#'   test statistics provided by the user. Alternative, the length of
#'   `alternative` should match the length of the `stats` parameter and it is
#'   assumed that there is a one-to-one correspondence. Defaults to
#'   `"right_tail"`.
#' @param combine_with A string specifying the combining function to be used to
#'   compute the single test statistic value from the set of p-value estimates
#'   obtained during the non-parametric combination testing procedure. For now,
#'   choices are either `"tippett"` or `"fisher"`. Defaults to `"tippett"`,
#'   which picks Tippett's function.
#' @param type A string specifying which formula should be used to compute the
#'   p-value. Choices are `exact`, `upper_bound` and `estimate`. See Phipson &
#'   Smith (2010) for details. Defaults to `"exact"`.
#' @param seed An integer specifying the seed of the random generator useful for
#'   result reproducibility or method comparisons. Default is `NULL`.
#' @param ... Extra parameters specific to some statistics.
#'
#' @return A [`base::list`] with 4 components:
#' - `observed`: the value of the (possible combined) test statistic(s) using
#' the original memberhips of data points;
#' - `pvalue`: the permutation p-value;
#' - `null_distribution`: the values of the (possible combined) test statistic(s)
#' using the permuted memberhips of data points;
#' - `permutations`: the permutations that were effectively sampled to produce
#' the null distribution.
#' @export
#'
#' @examples
#' out1 <- anova_test(
#'   data = dist(chickwts$weight),
#'   memberships = chickwts$feed,
#'   stats = list(stat_anova_f_ip)
#' )
#' out1$pvalue
#'
#' out2 <- anova_test(
#'   data = chickwts$weight,
#'   memberships = chickwts$feed,
#'   stats = list(stat_anova_f)
#' )
#' out2$pvalue
anova_test <- function(data, memberships,
                       stats = list(stat_anova_f_ip),
                       B = 1000L,
                       M = NULL,
                       alternative = "right_tail",
                       combine_with = "tippett",
                       type = "exact",
                       seed = NULL,
                       ...) {

  if (rlang::is_bare_numeric(data) || is.matrix(data) || is.data.frame(data))
    data <- purrr::array_tree(data, margin = 1)

  if (!is.list(data) && !inherits(data, "dist"))
    abort("The {.arg data} argument should be either of class {.cls numeric} or
          of class {.cls matrix} or of class {.cls list} or of class {.cls dist}.")

  if (!rlang::is_bare_integer(memberships) &&
      !rlang::is_bare_character(memberships) &&
      !is.factor(memberships))
    abort("The {.arg memberships} argument should be a factor specifying the class
          which each observation belongs to.")
  original_memberships <- unclass(as.factor(memberships))
  n <- length(memberships)

  # Compute total number of permutations yielding to distinct
  # values of the test statistic
  if (is.null(M))
    M <- factorial(n) - 1

  # Generate permutation data
  perm_data <- replicate(B, sample(original_memberships, size = n))
  perm_data <- cbind(original_memberships, perm_data)

  run_permutation_scheme(
    type = type,
    alternative = alternative,
    stats = stats,
    B = B,
    perm_data = perm_data,
    stat_data = data,
    M = M,
    combine_with = combine_with,
    ...
  )
}

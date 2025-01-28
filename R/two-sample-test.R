#' Two-Sample Permutation Test
#'
#' This function carries out an hypothesis test in which the null hypothesis is
#' that the two samples are governed by the same underlying generative
#' probability distribution against the alternative hypothesis that they are
#' governed by two different generative probability distributions.
#'
#' @section User-supplied statistic function:
#' A user-specified function should have at least two arguments:
#'
#' - the first argument is `data` which should be a list of the `n1 + n2`
#' concatenated observations with the original `n1` observations from the first
#' sample on top and the original `n2` observations from the second sample
#' below;
#' - the second argument is `perm_data` which should be an integer vector giving
#' the indices in `data` that are considered to belong to the first sample.
#'
#' It is possible to use the \code{\link{use_stat}} function with `nsamples = 2`
#' to have **flipr** automatically generate a template file for writing down
#' your own test statistics in a way that makes it compatible with the **flipr**
#' framework.
#'
#' See the \code{\link{stat_t}} function for an example.
#'
#' @param x A numeric vector or a numeric matrix or a list representing the 1st
#'   sample. Alternatively, it can be a distance matrix stored as an object of
#'   class \code{\link[stats]{dist}}, in which case test statistics based on
#'   inter-point distances (marked with the `_ip` suffix) should be used.
#' @param y A numeric vector if `x` is a numeric vector, or a numeric matrix if
#'   `x` is a numeric matrix, or a list if `x` is a list, representing the second
#'   sample. Alternatively, if `x` is an object of class
#'   \code{\link[stats]{dist}}, it should be a numeric scalar specifying the
#'   size of the first sample.
#' @param stats A list of functions produced by \code{\link[rlang]{as_function}}
#'   specifying the chosen test statistic(s). A number of test statistic
#'   functions are implemented in the package and can be used as such.
#'   Alternatively, one can provide its own implementation of test statistics
#'   that (s)he deems relevant for the problem at hand. See the section
#'   *User-supplied statistic function* for more information on how these
#'   user-supplied functions should be structured for compatibility with the
#'   **flipr** framework. Default is \code{list(\link{stat_t})}.
#' @param B The number of sampled permutations. Default is `1000L`.
#' @param M The total number of possible permutations. Defaults to `NULL`, which
#'   means that it is automatically computed from the given sample size(s).
#' @param alternative A single string or a character vector specifying whether
#'   the p-value is right-tailed, left-tailed or two-tailed. Choices are
#'   `"right_tail"`, `"left_tail"` and `"two_tail"`. Default is `"two_tail"`. If
#'   a single string is provided, it is assumed that it should be applied to all
#'   test statistics provided by the user. Alternative, the length of
#'   `alternative` should match the length of the `stats` parameter and it is
#'   assumed that there is a one-to-one correspondence.
#' @param combine_with A string specifying the combining function to be used to
#'   compute the single test statistic value from the set of p-value estimates
#'   obtained during the non-parametric combination testing procedure. For now,
#'   choices are either `"tippett"` or `"fisher"`. Default is `"tippett"`, which
#'   picks Tippett's function.
#' @param type A string specifying which formula should be used to compute the
#'   p-value. Choices are `exact` (default), `upper_bound` and `estimate`. See
#'   Phipson & Smith (2010) for details.
#' @param seed An integer specifying the seed of the random generator useful for
#'   result reproducibility or method comparisons. Default is `NULL`.
#' @param ... Extra parameters specific to some statistics.
#'
#' @return A \code{\link[base]{list}} with three components: the value of the
#'   statistic for the original two samples, the p-value of the resulting
#'   permutation test and a numeric vector storing the values of the permuted
#'   statistics.
#' @export
#'
#' @examples
#' n <- 10L
#' mx <- 0
#' sigma <- 1
#'
#' # Two different models for the two populations
#' x <- rnorm(n = n, mean = mx, sd = sigma)
#' delta <- 10
#' my <- mx + delta
#' y <- rnorm(n = n, mean = my, sd = sigma)
#' t1 <- two_sample_test(x, y)
#' t1$pvalue
#'
#' # Same model for the two populations
#' x <- rnorm(n = n, mean = mx, sd = sigma)
#' delta <- 0
#' my <- mx + delta
#' y <- rnorm(n = n, mean = my, sd = sigma)
#' t2 <- two_sample_test(x, y)
#' t2$pvalue
two_sample_test <- function(x, y,
                            stats = list(stat_t),
                            B = 1000L,
                            M = NULL,
                            alternative = "two_tail",
                            combine_with = "tippett",
                            type = "exact",
                            seed = NULL,
                            ...) {

  if (rlang::is_bare_numeric(x)) {
    if (!rlang::is_bare_numeric(y) || length(y) == 1)
      abort("When the first sample is of scalar type, the second sample should be of scalar type as well.")
  } else if (is.matrix(x)) {
    if (!is.matrix(y))
      abort("When the first sample is of vector type, the second sample should be of vector type as well.")
  } else if (is.list(x)) {
    if (!is.list(y))
      abort("When the first sample is stored in a list, the second sample should be stored in a list as well.")
  } else if (inherits(x, "dist")) {
    if (!rlang::is_scalar_integer(y))
      abort("When the first argument is a distance matrix, the second argument should be an integer specifying the size of the first sample.")
  } else {
    abort("The first argument should be of class numeric, matrix, list or dist.")
  }

  if (!is.null(seed)) withr::local_seed(seed)

  if (inherits(x, "dist")) {
    stat_data <- x
    n <- attr(x, "Size")
    n1 <- y
    n2 <- n - n1
  } else {
    #l <- convert_to_list(x, y)
    #x <- l[[1]]
    #y <- l[[2]]
    n1 <- length(x)
    n2 <- length(y)
    n <- n1 + n2
    stat_data <- c(x, y)
  }

  # Compute total number of permutations yielding to distinct
  # values of the test statistic
  if (is.null(M))
    M <- choose(n, n1) - 1

  # Generate permutation data
  if (M <= B) {
    B <- M
    perm_data <- utils::combn(n, n1)[, 1:B + 1]
  } else {
    perm_data <- replicate(B, sample.int(n))[1:n1, ]
  }
  perm_data <- cbind(
    seq_len(nrow(perm_data)),
    perm_data
  )

  run_permutation_scheme(
    type = type,
    alternative = alternative,
    stats = stats,
    B = B,
    perm_data = perm_data,
    stat_data = stat_data,
    M = M,
    combine_with = combine_with,
    ...
  )
}

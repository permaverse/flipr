#' Test Statistics for the Two-Sample Problem
#'
#' This is a collection of functions that provide test statistics to be used
#' into the permutation scheme for performing two-sample testing. These test
#' statistics can be divided into two categories: traditional statistics that
#' use empirical moments and inter-point statistics that only rely on pairwise
#' dissimilarities between data points.
#'
#' @section Traditional Test Statistics:
#'
#' - \code{\link{stat_hotelling}} implements Hotelling's \eqn{T^2} statistic for
#' multivariate data with \eqn{p < n}.
#' - \code{\link{stat_student}} or \code{\link{stat_t}} implements Student's
#' statistic (originally assuming equal variances and thus using the pooled
#' empirical variance estimator). See \code{\link[stats]{t.test}} for details.
#' - \code{\link{stat_welch}} implements Student-Welch statistic which is
#' essentially a modification of Student's statistic accounting for unequal
#' variances. See \code{\link[stats]{t.test}} for details.
#' - \code{\link{stat_fisher}} or \code{\link{stat_f}} implements Fisher's
#' variance ratio statistic. See \code{\link[stats]{var.test}} for details.
#' - \code{\link{stat_mean}} implements a statistic that computes the difference
#' between the means.
#' - \code{\link{stat_bs}} implements the statistic proposed by Bai & Saranadasa
#' (1996) for high-dimensional multivariate data.
#'
#' @section Inter-Point Test Statistics:
#'
#' - \code{\link{stat_student_ip}} or \code{\link{stat_t_ip}} implements a
#' Student-like test statistic based on inter-point distances only as described
#' in Lovato et al. (2020).
#' - \code{\link{stat_fisher_ip}} or \code{\link{stat_f_ip}} implements a
#' Fisher-like test statistic based on inter-point distances only as described
#' in Lovato et al. (2020).
#' - \code{\link{stat_bg_ip}} implements the statistic proposed by Biswas &
#' Ghosh (2014).
#' - \code{\link{stat_energy_ip}} implements the class of energy-based
#' statistics as described in Székely & Rizzo (2013);
#' - \code{\link{stat_cq_ip}} implements the statistic proposed by Chen & Qin
#' (2010).
#' - \code{\link{stat_mod_ip}} implements a statistic that computes the mean of
#' inter-point distances.
#' - \code{\link{stat_dom_ip}} implements a statistic that computes the distance
#' between the medoids of the two samples, possibly standardized by the pooled
#' corresponding variances.
#'
#' @references
#' Bai, Z., & Saranadasa, H. (1996). Effect of high dimension: by an example of
#' a two sample problem. Statistica Sinica, 311-329.
#'
#' Lovato, I., Pini, A., Stamm, A., & Vantini, S. (2020). Model-free two-sample
#' test for network-valued data. Computational Statistics & Data Analysis, 144,
#' 106896.
#'
#' Biswas, M., & Ghosh, A. K. (2014). A nonparametric two-sample test applicable
#' to high dimensional data. Journal of Multivariate Analysis, 123, 160-171.
#'
#' Székely, G. J., & Rizzo, M. L. (2013). Energy statistics: A class of
#' statistics based on distances. Journal of statistical planning and inference,
#' 143(8), 1249-1272.
#'
#' Chen, S. X., & Qin, Y. L. (2010). A two-sample test for high-dimensional data
#' with applications to gene-set testing. The Annals of Statistics, 38(2),
#' 808-835.
#'
#' @param data Either a list of the `n1 + n2` concatenated observations with the
#'   original `n1` observations from the first sample on top and the original
#'   `n2` observations from the second sample below. Or a dissimilarity matrix
#'   stored as a \code{\link[stats]{dist}} object for all inter-point statistics
#'   whose function name should end with `_ip()`.
#' @param indices1 An integer vector specifying the indices in `data` that are
#'   considered to belong to the first sample.
#' @param alpha A scalar value specifying the power to which the dissimilarities
#'   should be elevated in the computation of the inter-point energy statistic.
#'   Default is `1L`.
#' @param standardize A boolean specifying whether the distance between medoids
#'   in the \code{\link{stat_dom_ip}} function should be normalized by the
#'   pooled corresponding variances. Default is `TRUE`.
#' @param ... Extra parameters specific to some statistics.
#'
#' @return A real scalar giving the value of test statistic for the permutation
#'   specified by the integer vector `indices`.
#' @name two-sample-stats
#'
#' @examples
#' n <- 10L
#' mx <- 0
#' sigma <- 1
#' delta <- 10
#' my <- mx + delta
#' x <- rnorm(n = n, mean = mx, sd = sigma)
#' y <- rnorm(n = n, mean = my, sd = sigma)
#' D <- dist(c(x, y))
#'
#' x <- as.list(x)
#' y <- as.list(y)
#'
#' stat_welch(c(x, y), 1:n)
#' stat_t(c(x, y), 1:n)
#' stat_f(c(x, y), 1:n)
#' stat_mean(c(x, y), 1:n)
#' stat_hotelling(c(x, y), 1:n)
#' stat_bs(c(x, y), 1:n)
#'
#' stat_t_ip(D, 1:n)
#' stat_f_ip(D, 1:n)
#' stat_bg_ip(D, 1:n)
#' stat_energy_ip(D, 1:n)
#' stat_cq_ip(D, 1:n)
#' stat_mod_ip(D, 1:n)
#' stat_dom_ip(D, 1:n)
NULL

#' @rdname two-sample-stats
#' @export
stat_welch <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  x1 <- unlist(data[l$idx1])
  x2 <- unlist(data[l$idx2])
  as.numeric(stats::t.test(x1, x2, var.equal = FALSE)$statistic)
}

#' @rdname two-sample-stats
#' @export
stat_student <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  x1 <- unlist(data[l$idx1])
  x2 <- unlist(data[l$idx2])
  as.numeric(stats::t.test(x2, x1, var.equal = TRUE)$statistic)
}

#' @rdname two-sample-stats
#' @export
stat_t <- stat_student

#' @rdname two-sample-stats
#' @export
stat_fisher <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  x1 <- unlist(data[l$idx1])
  x2 <- unlist(data[l$idx2])
  as.numeric(stats::var.test(x2, x1)$statistic)
}

#' @rdname two-sample-stats
#' @export
stat_f <- stat_fisher

#' @rdname two-sample-stats
#' @export
stat_mean <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  x1 <- unlist(data[l$idx1])
  x2 <- unlist(data[l$idx2])
  mean(x1) - mean(x2)
}

#' @rdname two-sample-stats
#' @export
stat_hotelling <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  X <- purrr::reduce(data[l$idx1], rbind)
  Y <- purrr::reduce(data[l$idx2], rbind)
  nx <- length(l$idx1)
  ny <- length(l$idx2)
  Xbar <- colMeans(X)
  Ybar <- colMeans(Y)
  Sx <- stats::cov(X)
  Sy <- stats::cov(Y)
  Spooled <- ((nx - 1) * Sx + (ny - 1) * Sy) / (nx + ny - 2)
  Sinv <- solve(Spooled)
  D <- Xbar - Ybar
  as.numeric(t(D) %*% Sinv %*% D)
}

#' @rdname two-sample-stats
#' @export
stat_bs <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  X <- purrr::reduce(data[l$idx1], rbind)
  Y <- purrr::reduce(data[l$idx2], rbind)
  nx <- length(l$idx1)
  ny <- length(l$idx2)
  Xbar <- colMeans(X)
  Ybar <- colMeans(Y)
  Sx <- stats::cov(X)
  Sy <- stats::cov(Y)
  Sn <- ((nx - 1) * Sx + (ny - 1) * Sy) / (nx + ny)
  D <- Xbar - Ybar
  as.numeric(t(D) %*% D) - (nx + ny) / (nx * ny) * sum(diag(Sn))
}

#' @rdname two-sample-stats
#' @export
stat_student_ip <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  stat_student_impl(data, l$idx1, l$idx2)
}

#' @rdname two-sample-stats
#' @export
stat_t_ip <- stat_student_ip

#' @rdname two-sample-stats
#' @export
stat_fisher_ip <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  stat_fisher_impl(data, l$idx1, l$idx2)
}

#' @rdname two-sample-stats
#' @export
stat_f_ip <- stat_fisher_ip

#' @rdname two-sample-stats
#' @export
stat_bg_ip <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  stat_bg_impl(data, l$idx1, l$idx2)
}

#' @rdname two-sample-stats
#' @export
stat_energy_ip <- function(data, indices1, alpha = 1L, ...) {
  l <- two_sample_prep(data, indices1)
  stat_energy_impl(data, l$idx1, l$idx2, alpha)
}

#' @rdname two-sample-stats
#' @export
stat_cq_ip <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  stat_cq_impl(data, l$idx1, l$idx2)
}

#' @rdname two-sample-stats
#' @export
stat_mod_ip <- function(data, indices1, ...) {
  l <- two_sample_prep(data, indices1)
  l <- expand.grid(idx1 = l$idx1, idx2 = l$idx2)
  dist_values <- purrr::map2_dbl(l$idx1, l$idx2, ~ getElement(
    distObject = data,
    rowIndex = .x,
    colIndex = .y
  ))
  mean(dist_values)
}

#' @rdname two-sample-stats
#' @export
stat_dom_ip <- function(data, indices1, standardize = TRUE, ...) {
  l <- two_sample_prep(data, indices1)
  n1 <- length(l$idx1)
  n2 <- length(l$idx2)

  ssd1_vec <- purrr::map_dbl(l$idx1, function(idx) {
    sum(purrr::map_dbl(l$idx1, ~ getElement(
      distObject = data,
      rowIndex = idx,
      colIndex = .x
    ))^2)
  })
  km1 <- l$idx1[which.min(ssd1_vec)]

  ssd2_vec <- purrr::map_dbl(l$idx2, function(idx) {
    sum(purrr::map_dbl(l$idx2, ~ getElement(
      distObject = data,
      rowIndex = idx,
      colIndex = .x
    ))^2)
  })
  km2 <- l$idx2[which.min(ssd2_vec)]

  stat <- getElement(data, km1, km2)

  if (!standardize)
    return(stat)

  ssd1 <- min(ssd1_vec)
  ssd2 <- min(ssd2_vec)
  pooled_variance <- (ssd1 + ssd2) / (n1 + n2 - 2)
  stat / sqrt(pooled_variance)
}

two_sample_prep <- function(data, indices1) {
  n <- if (inherits(data, "dist"))
    attr(data, "Size")
  else if (inherits(data, "list"))
    length(data)
  else
    stop("The `data` input should be of class either list or dist.")
  indices2 <- seq_len(n)[-indices1]
  list(idx1 = indices1, idx2 = indices2)
}

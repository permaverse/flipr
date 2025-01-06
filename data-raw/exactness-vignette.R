## code to prepare `flipr-vignette` dataset goes here

library(purrr)
library(parallel)
library(furrr)
library(flipr)

# Parallelization
future::plan("future::multisession", workers = availableCores())

# General setup
nreps <- 1e4
n1 <- 5
n2 <- 5
set.seed(12345)
sim <- map(sample.int(.Machine$integer.max, nreps, replace = TRUE), ~ {
    list(
      x = rnorm(n = n1, mean = 0, sd = 1),
      y = rnorm(n = n2, mean = 0, sd = 1),
      s = .x
    )
  })

null_spec <- function(y, parameters) {
  map(y, ~ .x - parameters)
}
stat_functions <- list(stat_t)
stat_assignments <- list(delta = 1)
nperms <- 20
alpha <- 0.05


progressr::with_progress({
  p <- progressr::progressor(steps = length(sim) / 10) # progress bar set up
  ii <- 1

  alpha_estimates <- furrr::future_map(sim, function(.l) {
    if (ii %% 10 == 0) {p()} # progress bar update
    ii <<- ii + 1

    pf <- PlausibilityFunction$new(
      null_spec = null_spec,
      stat_functions = stat_functions,
      stat_assignments = stat_assignments,
      .l$x, .l$y,
      seed = .l$s
    )
    pf$set_nperms(nperms)
    pf$set_alternative("left_tail")
    pf$set_pvalue_formula("exact")
    pv_exact <- pf$get_value(0)
    pf$set_pvalue_formula("upper_bound")
    pv_upper_bound <- pf$get_value(0)
    pf$set_pvalue_formula("estimate")
    pv_estimate <- pf$get_value(0)
    c(
      exact       = pv_exact       <= alpha,
      upper_bound = pv_upper_bound <= alpha,
      estimate    = pv_estimate    <= alpha
    )
  }, .options = furrr_options(seed = TRUE)) %>%
    transpose() %>%
    simplify_all() %>%
    map(mean)
})


alpha_estimates

saveRDS(alpha_estimates, "data-raw/alpha.rds")

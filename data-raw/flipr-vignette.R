## code to prepare `flipr-vignette` dataset goes here

# Setup -------------------------------------------------------------------

library(purrr)
library(flipr)

n1 <- 10
n2 <- 10
mu1 <- 1
mu2 <- 4
sd1 <- 1
sd2 <- 1
nperms <- 100000

null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}
stat_functions <- list(stat_t)
stat_assignments <- list(delta = 1)

# Scenario A --------------------------------------------------------------

set.seed(1234)
a1 <- rnorm(n1, mean = mu1, sd = sd1)
a2 <- rnorm(n2, mean = mu2, sd = sd2)

pfa <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  a1, a2
)

pfa$set_point_estimate(mean(a2) - mean(a1))

pfa$set_nperms(10000)
pfa$set_parameter_bounds(
  point_estimate = pfa$point_estimate,
  conf_level = pfa$max_conf_level
)

pfa$set_grid(
  parameters = pfa$parameters,
  npoints = 50L
)

pfa$set_nperms(nperms)
pfa$evaluate_grid(grid = pfa$grid)

saveRDS(pfa, "inst/vignette-data/flipr-pfa.rds")

# Scenario B --------------------------------------------------------------

set.seed(1234)
b1 <- rgamma(n1, shape = 1, rate = 1)
b2 <- rgamma(n2, shape = 16, rate = 4)

pfb <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  b1, b2
)

pfb$set_point_estimate(mean(b2) - mean(b1))

pfb$set_nperms(10000)
pfb$set_parameter_bounds(
  point_estimate = pfb$point_estimate,
  conf_level = pfb$max_conf_level
)

pfb$set_grid(
  parameters = pfb$parameters,
  npoints = 50L
)

pfb$set_nperms(nperms)
pfb$evaluate_grid(grid = pfb$grid)

saveRDS(pfb, "inst/vignette-data/flipr-pfb.rds")

# Scenario C --------------------------------------------------------------

set.seed(1234)
c1 <- rnorm(n1, mean = mu1, sd = sd1)
c2 <- rgamma(n2, shape = 16, rate = 4)

pfc <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  c1, c2
)

pfc$set_point_estimate(mean(c2) - mean(c1))

pfc$set_nperms(10000)
pfc$set_parameter_bounds(
  point_estimate = pfc$point_estimate,
  conf_level = pfc$max_conf_level
)

pfc$set_grid(
  parameters = pfc$parameters,
  npoints = 50L
)

pfc$set_nperms(nperms)
pfc$evaluate_grid(grid = pfb$grid)

saveRDS(pfc, "inst/vignette-data/flipr-pfc.rds")

## code to prepare `parallelization-vignette` dataset goes here

# Setup -------------------------------------------------------------------

library(future)
library(parallel)
library(progressr)
library(tictoc)
library(flipr)

ngrid_in <- 50L
nperms <- 5000
n1 <- 10
set.seed(1234)
x <- rnorm(n1, mean = 1, sd = 1)
y <- rnorm(n1, mean = 4, sd = 1)

null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}

stat_functions <- list(stat_t)

stat_assignments <- list(delta = 1)

# Inference on the mean without parallelization --------------------------------

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)

pf$set_point_estimate(mean(y) - mean(x))

pf$set_nperms(nperms)
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = pf$max_conf_level
)

pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_in
)

tic()
pf$evaluate_grid(grid = pf$grid)
time_without_parallelization <- toc()$callback_msg

# Inference on the mean with parallelization -----------------------------------

ncores <- 4
plan(multisession, workers = ncores)
cl <- makeCluster(ncores)
setDefaultCluster(cl)
progressr::handlers(global = TRUE)

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)

pf$set_point_estimate(mean(y) - mean(x))

pf$set_nperms(nperms)
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = pf$max_conf_level
)

pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_in
)

tic()
pf$evaluate_grid(grid = pf$grid)
time_with_parallelization <- toc()$callback_msg

df_parallelization <- list(
  delta = pf$grid$delta,
  time_par = time_with_parallelization,
  time_without_par = time_without_parallelization
)

saveRDS(df_parallelization, "data-raw/df_parallelization.rds")


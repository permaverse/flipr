## code to prepare `parallelization-vignette` dataset goes here

# Setup -------------------------------------------------------------------

library(future)
library(parallel)
library(progressr)
library(tictoc)
library(flipr)

ngrid_in <- 100L
nperms <- 2000
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

plan(sequential)
setDefaultCluster(NULL)
progressr::handlers(global = FALSE)

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x, y
)
pf$set_nperms(nperms)

tic()
pf$set_point_estimate()
time_without_parallel <- toc()$callback_msg

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
time_without_future <- toc()$callback_msg

# Inference on the mean with parallelization -----------------------------------

ncores <- 3
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
pf$set_nperms(nperms)

tic()
pf$set_point_estimate()
time_with_parallel <- toc()$callback_msg

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
time_with_future <- toc()$callback_msg

stopCluster(cl)

df_parallelization <- list(
  delta = pf$grid$delta,
  time_without_parallel = time_without_parallel,
  time_without_future = time_without_future,
  time_with_parallel = time_with_parallel,
  time_with_future = time_with_future
)

saveRDS(df_parallelization, "inst/vignette-data/parallelization-df.rds")

## code to prepare `plausibility-vignette` dataset goes here

# Setup -------------------------------------------------------------------

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(flipr)

ngrid_in <- 10
ngrid_out <- 100
nperms <- 100000
n1 <- 30
n2 <- 30
set.seed(1301)
x1 <- rnorm(n1, mean = 0, sd = 1)
x2 <- rnorm(n2, mean = 3, sd = 1)
y1 <- rnorm(n1, mean = 0, sd = 1)
y2 <- rnorm(n2, mean = 0, sd = 2)
z1 <- rnorm(n1, mean = 0, sd = 1)
z2 <- rnorm(n2, mean = 3, sd = 2)

# Inference on the mean ---------------------------------------------------

null_spec <- function(y, parameters) {
  map(y, ~ .x - parameters)
}
stat_functions <- list(stat_t)
stat_assignments <- list(delta = 1)

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  x1, x2,
  seed = 1234
)

pf$set_point_estimate(mean(x2) - mean(x1))

pf$set_nperms(10000)
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = pf$max_conf_level
)

pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_in
)

pf$set_nperms(nperms)

pf$set_alternative("two_tail")
pf$evaluate_grid(grid = pf$grid)
df <- rename(pf$grid, two_tail = pvalue)

pf$set_alternative("left_tail")
pf$grid$pvalue <- NULL
pf$evaluate_grid(grid = pf$grid)
df <- bind_rows(
  df,
  rename(pf$grid, left_tail = pvalue)
)

pf$set_alternative("right_tail")
pf$grid$pvalue <- NULL
pf$evaluate_grid(grid = pf$grid)
df <- bind_rows(
  df,
  rename(pf$grid, right_tail = pvalue)
)

pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_out
)

df_mean <- tibble(
  delta = pf$grid$delta,
  two_tail = approx(df$delta, df$two_tail, delta)$y,
  left_tail = approx(df$delta, df$left_tail, delta)$y,
  right_tail = approx(df$delta, df$right_tail, delta)$y,
) %>%
  pivot_longer(-delta)

saveRDS(df_mean, "inst/vignette-data/plausibility-df-mean.rds")

# Inference on the standard deviation -------------------------------------

null_spec <- function(y, parameters) {
  map(y, ~ .x / parameters)
}
stat_functions <- list(stat_f)
stat_assignments <- list(rho = 1)

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  y1, y2,
  seed = 1234
)

pf$set_point_estimate(sd(y2) / sd(y1))

pf$set_nperms(10000)
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = pf$max_conf_level
)

pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_in
)

pf$set_nperms(nperms)

pf$set_alternative("two_tail")
pf$evaluate_grid(grid = pf$grid)
df <- rename(pf$grid, two_tail = pvalue)

pf$set_alternative("left_tail")
pf$grid$pvalue <- NULL
pf$evaluate_grid(grid = pf$grid)
df <- bind_rows(
  df,
  rename(pf$grid, left_tail = pvalue)
)

pf$set_alternative("right_tail")
pf$grid$pvalue <- NULL
pf$evaluate_grid(grid = pf$grid)
df <- bind_rows(
  df,
  rename(pf$grid, right_tail = pvalue)
)

pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_out
)

df_sd <- tibble(
  rho = pf$grid$rho,
  two_tail = approx(df$rho, df$two_tail, rho)$y,
  left_tail = approx(df$rho, df$left_tail, rho)$y,
  right_tail = approx(df$rho, df$right_tail, rho)$y,
) %>%
  pivot_longer(-rho)

saveRDS(df_sd, "inst/vignette-data/plausibility-df-sd.rds")

# Inference on both the mean and the standard deviation -------------------

null_spec <- function(y, parameters) {
  map(y, ~ (.x - parameters[1]) / exp(parameters[2]))
}
stat_functions <- list(stat_t, stat_f)
stat_assignments <- list(delta = 1, log_rho = 2)

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  z1, z2,
  seed = 1234
)

pf$set_point_estimate(c(
  mean(z2) - sd(z2) / sd(z1) * mean(z1),
  log(sd(z2) / sd(z1))
))

pf$set_nperms(10000)
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = pf$max_conf_level
)

pf$set_nperms(nperms)

# Fisher combining function
pf$set_aggregator("fisher")
pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_in
)
pf$evaluate_grid(grid = pf$grid)
grid_in <- pf$grid
pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_out
)
if (requireNamespace("interp", quietly = TRUE)) {
  Zout <- interp::interp(
    x = grid_in$delta,
    y = grid_in$log_rho,
    z = grid_in$pvalue,
    xo = sort(unique(pf$grid$delta)),
    yo = sort(unique(pf$grid$log_rho))
  )
  pf$grid$pvalue <- as.numeric(Zout$z)
} else
  pf$grid$pvalue <- rep(NA, nrow(pf$grid))

df_fisher <- pf$grid

saveRDS(df_fisher, "inst/vignette-data/plausibility-df-fisher.rds")

# Tippett combining function
pf$set_aggregator("tippett")
pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_in
)
pf$evaluate_grid(grid = pf$grid)
grid_in <- pf$grid
pf$set_grid(
  parameters = pf$parameters,
  npoints = ngrid_out
)
if (requireNamespace("interp", quietly = TRUE)) {
  Zout <- interp::interp(
    x = grid_in$delta,
    y = grid_in$log_rho,
    z = grid_in$pvalue,
    xo = sort(unique(pf$grid$delta)),
    yo = sort(unique(pf$grid$log_rho))
  )
  pf$grid$pvalue <- as.numeric(Zout$z)
} else
  pf$grid$pvalue <- rep(NA, nrow(pf$grid))

df_tippett <- pf$grid

saveRDS(df_tippett, "inst/vignette-data/plausibility-df-tippett.rds")

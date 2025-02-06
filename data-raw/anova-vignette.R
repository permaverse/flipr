## code to prepare `anova-vignette` dataset goes here

# Setup -------------------------------------------------------------------

library(future)
library(parallel)
library(progressr)
library(flipr)
library(dplyr)

ncores <- 4
plan(multisession, workers = ncores)
cl <- makeCluster(ncores)
setDefaultCluster(cl)

# 1-ANOVA -------------------------------------------------------------------

data <- chickwts$weight[1:36] # 1 variable
memberships <- as.factor(chickwts$feed[1:36]) # 1 factor with 3 populations

# takes all samples except the first one
null_spec <- function(l, parameters) {
  purrr::map2(l, parameters, \(l, param) l - param)
}
stat_functions <- list(stat_anova_f)

# all means except first one
stat_assignments <- list(mu2 = 1, mu3 = 1)

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  data, memberships
)
pf$set_alternative("right_tail")

samples <- purrr::map(unique(as.numeric(memberships)), \(.class) {
  data[which(as.numeric(memberships) == .class)]
})
means <- c(mean(samples[[2]]) - mean(samples[[1]]),
           mean(samples[[3]]) - mean(samples[[1]]))
pf$set_point_estimate(means)
pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = pf$max_conf_level
)
pf$set_grid(
  parameters = pf$parameters,
  npoints = 10
)
pf$evaluate_grid(grid = pf$grid)

out <- list(
  pvalue = pf$get_value(rep(0, 2)),
  pvalue_est = pf$get_value(means),
  point_estimate = pf$point_estimate,
  grid = pf$grid
)

saveRDS(out, "data-raw/df_1anova.rds")


# M-ANOVA -------------------------------------------------------------------

iris_setosa <- iris |> filter(Species == "setosa")
iris_setosa <- iris_setosa[1:10,1:2]
iris_versicolor <- iris |> filter(Species == "versicolor")
iris_versicolor <- iris_versicolor[1:10,1:2]
iris_virginica <- iris |> filter(Species == "virginica")
iris_virginica <- iris_virginica[1:10,1:2]

# takes all samples except the first one
null_spec <- function(l, parameters) {
  parameters <- split(parameters, ceiling(seq_along(parameters) / (length(parameters)/length(l))))
  purrr::map2(l, parameters, \(group, params) {
    purrr::map2(group, params, \(var, param) {
      var - param
    }) |> as.data.frame()
  })
}
stat_functions <- list(stat_anova_f)

# all means except ones from first sample
stat_assignments <- list(
  mu2_1 = 1, mu2_2 = 1,
  mu3_1 = 1, mu3_2 = 1)

pf <- PlausibilityFunction$new(
  null_spec = null_spec,
  stat_functions = stat_functions,
  stat_assignments = stat_assignments,
  iris_setosa, iris_versicolor, iris_virginica
)
pf$set_alternative("right_tail")

d1 <- c(
  mean(iris_versicolor$Sepal.Length) - mean(iris_setosa$Sepal.Length),
  mean(iris_versicolor$Sepal.Width)  - mean(iris_setosa$Sepal.Width)
)
d2 <- c(
  mean(iris_virginica$Sepal.Length) - mean(iris_setosa$Sepal.Length),
  mean(iris_virginica$Sepal.Width)  - mean(iris_setosa$Sepal.Width)
)
pf$set_point_estimate(c(d1, d2))

pf$set_parameter_bounds(
  point_estimate = pf$point_estimate,
  conf_level = pf$max_conf_level
)

pf$set_grid(
  parameters = pf$parameters,
  npoints = 10
)

pf$evaluate_grid(grid = pf$grid)

out <- list(
  pvalue = pf$get_value(rep(0, 4)),
  pvalue_est = pf$get_value(c(d1, d2)),
  point_estimate = pf$point_estimate,
  grid = pf$grid
)

saveRDS(out, "data-raw/df_2anova.rds")


stopCluster(cl)


#' R6 Class representing a plausibility function
#'
#' @description A plausibility function is...
#'
#' @export
PlausibilityFunction <- R6::R6Class(
  classname = "PlausibilityFunction",
  public = list(
    #' @description Create a new plausibility function object.
    #'
    #' @param null_spec A function or an R object coercible into a function (via
    #'   `rlang::as_function()`). For one-sample problems, it should transform
    #'   the `x` sample (provided as first argument) using the parameters (as
    #'   second argument) to make its distribution centered symmetric. For
    #'   two-sample problems, it should transform the `y` sample (provided as
    #'   first argument) using the parameters (as second argument) to make it
    #'   exchangeable with the `x` sample under a null hypothesis.
    #' @param stat_functions A vector or list of functions (or R objects
    #'   coercible into functions via `rlang::as_function()`) specifying the
    #'   whole set of test statistics that should be used.
    #' @param stat_assignments A named list of integer vectors specifying which
    #'   test statistic should be associated with each parameter. The length of
    #'   this list should match the number of parameters under investigation and
    #'   is thus used to set it. Each element of the list should be named after
    #'   the parameter it identifies.
    #' @param ... Vectors, matrices or lists providing the observed samples.
    #' @param seed A numeric value specifying the seed to be used. Defaults to
    #'   `NULL` in which case `seed = 1234` is used and the user is informed of
    #'   this setting.
    #'
    #' @return A new `PlausibilityFunction` object.
    initialize = function(null_spec,
                          stat_functions,
                          stat_assignments,
                          ...,
                          seed = NULL) {
      if (!is_function(null_spec))
        abort("The `null_spec` argument should be of class `function`.")
      private$set_null_spec(null_spec)

      private$set_stat_functions(stat_functions)

      if (!is_list(stat_assignments))
        abort("The `stat_assignements` argument should be of class `list`.")
      if (!is_named(stat_assignments))
        abort("The `stat_assignments` list should be a named list`.")
      private$set_stat_assignments(stat_assignments)
      private$set_nparams(length(stat_assignments))

      private$set_data(...)

      param_names <- names(stat_assignments)
      self$parameters <- list2()
      for (param in param_names) {
        param_label <- format_param_label(param)
        self$parameters <- c(
          self$parameters,
          list2(!!param := eval_tidy(expr(new_inferred_param(
              type = "double",
              label = unlist(list2(!!param := !!param_label)),
              finalize = get_ci
            )
          )))
        )
      }
      self$point_estimate <- get_point_estimate(self$parameters)

      if (is.null(seed)) {
        cli::cli_alert_warning(
          "Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`."
        )
        seed <- 1234
      }
      self$seed <- seed
    },

    #' @field nparams An integer specifying the number of parameters to be
    #'   inferred. Default is `1L`.
    nparams = 1,

    #' @field nperms An integer specifying the number of permutations to be
    #'   sampled. Default is `1000L`.
    nperms = 1000L,

    #' @description Change the value of the `nperms` field.
    #'
    #' @param val New value for the number of permutations to be sampled.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$nperms
    #' pf$set_nperms(10000)
    #' pf$nperms
    set_nperms = function(val) {
      self$nperms <- val
    },

    #' @field nperms_max An integer specifying the total number of distinct
    #'   permutations that can be made given the sample sizes.
    nperms_max = NULL,

    #' @description Change the value of the `nperms_max` field.
    #'
    #' @param val New value for the total number of of possible distinct
    #'   permutations.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$nperms_max
    #' pf$set_nperms_max(10000)
    #' pf$nperms_max
    set_nperms_max = function(val) {
      self$nperms_max <- val
    },

    #' @field alternative A string specifying the type of alternative
    #'   hypothesis. Choices are `"two_tail"`, `"left_tail"` and `"right_tail`.
    #'   Defaults to `"two_tail"`.
    alternative = "two_tail",

    #' @description Change the value of the `alternative` field.
    #'
    #' @param val New value for the type of alternative hypothesis.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$alternative
    #' pf$set_alternative("right_tail")
    #' pf$alternative
    set_alternative = function(val) {
      if (!(val %in% private$alternative_choices))
        abort(paste0(
          "The `alternative` argument should be one of ",
          private$alternative_choices,
          "."
        ))
      self$alternative <- val
    },

    #' @field aggregator A string specifying which function should be used to
    #'   aggregate test statistic values when non-parametric combination is used
    #'   (i.e. when multiple test statistics are used). Choices are `"tippett"`
    #'   and `"fisher` for now. Defaults to `"tippett"`.
    aggregator = "tippett",

    #' @description Change the value of the `aggregator` field.
    #'
    #' @param val New value for the string specifying which function should be
    #'   used to aggregate test statistic values when non-parametric combination
    #'   is used (i.e. when multiple test statistics are used).
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$aggregator
    #' pf$set_aggregator("fisher")
    #' pf$aggregator
    set_aggregator = function(val) {
      if (!(val %in% private$aggregator_choices))
        abort(paste0(
          "The `aggregator` argument should be one of ",
          private$aggregator_choices,
          "."
        ))
      self$aggregator <- val
    },

    #' @field pvalue_formula A string specifying which formula to use for
    #'   computing the permutation p-value. Choices are either `exact`
    #'   (default), `upper_bound` or `estimate`. The former provides p-values that lead to
    #'   exact hypothesis tests while the latter provides an unbiased estimate
    #'   of the traditional p-value.
    pvalue_formula = "exact",

    #' @description Change the value of the `pvalue_formula` field.
    #'
    #' @param val New value for the string specifying which formula should be
    #'   used to compute the permutation p-value.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$pvalue_formula
    #' pf$set_pvalue_formula("estimate")
    #' pf$pvalue_formula
    set_pvalue_formula = function(val) {
      if (!(val %in% private$pvalue_formula_choices))
        abort(paste0(
          "The `pvalue_formula` argument should be one of ",
          private$pvalue_formula_choices,
          "."
        ))
      self$pvalue_formula <- val
    },

    #' @description Computes an indicator of the plausibility of specific values
    #'   for the parameters of interest in the form of a p-value of an
    #'   hypothesis test against these values.
    #'
    #' @param parameters A vector whose length should match the `nparams` field
    #'   providing specific values of the parameters of interest for assessment
    #'   of their plausibility in the form of a p-value of the corresponding
    #'   hypothesis test.
    #' @param keep_null_distribution A boolean specifying whether the empirical
    #'   permutation null distribution should be returned as well. Defaults to
    #'   `FALSE`.
    #' @param keep_permutations A boolean specifying whether the list of sampled
    #'   permutations used to compute the empirical permutation null
    #'   distribution should be returned as well. Defaults to `FALSE`.
    #' @param ... Extra parameters specific to some statistics.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$set_nperms(50)
    #' pf$get_value(2)
    get_value = function(parameters,
                         keep_null_distribution = FALSE,
                         keep_permutations = FALSE,
                         ...) {
      if (length(parameters) != self$nparams)
        abort(paste0(
          "The plausibility function has been defined to infer ",
          self$nparams,
          " parameters and you are trying to evaluate it for a vector of parameters of length ",
          length(parameters),
          "."
        ))
      withr::local_seed(self$seed)
      if (private$nsamples == 1) {
        x <- private$null_spec(unlist(private$data[[1]]), parameters)
        test_result <- one_sample_test(
          x = x,
          stats = private$stat_functions,
          B = self$nperms,
          M = self$nperms_max,
          alternative = self$alternative,
          type = self$pvalue_formula,
          combine_with = self$aggregator,
          ...
        )
      } else if(private$nsamples == 2) {
        y <- unlist(private$data[[1]][which(private$data[[2]] == 2)])
        y <- private$null_spec(y, parameters)
        test_result <- two_sample_test(
          x = private$data[[1]][which(private$data[[2]] == 1)],
          y = y,
          stats = private$stat_functions,
          B = self$nperms,
          M = self$nperms_max,
          alternative = self$alternative,
          type = self$pvalue_formula,
          combine_with = self$aggregator,
          ...
        )
      } else if(private$nsamples > 2) {
        test_result <- anova_test(
          data = private$data[[1]],
          memberships = private$data[[2]],
          stats = private$stat_functions,
          B = self$nperms,
          M = self$nperms_max,
          alternative = self$alternative,
          type = self$pvalue_formula,
          combine_with = self$aggregator,
          ...
        )
      }

      #TODO Regression

      if (keep_null_distribution && keep_permutations)
        return(test_result)
      else if (keep_null_distribution) {
        test_result$permutations <- NULL
        return(test_result)
      } else if (keep_permutations) {
        test_result$null_distribution <- NULL
        return(test_result)
      } else
        return(test_result$pvalue)
    },

    #' @field max_conf_level A numeric value specifying the maximum confidence
    #'   level that we aim to achieve for the confidence regions. This is used
    #'   to compute bounds on each parameter of interest in order to fit a
    #'   Kriging model that approximates the expensive plausibility function on
    #'   a hypercube. Defaults to `0.99`.
    max_conf_level = 0.99,

    #' @description Change the value of the `max_conf_level` field.
    #'
    #' @param val New value for the maximum confidence level that we aim to
    #'   achieve for the confidence regions.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$max_conf_level
    #' pf$set_max_conf_level(0.999)
    #' pf$max_conf_level
    set_max_conf_level = function(val) {
      self$max_conf_level <- val
    },

    #' @field point_estimate A numeric vector providing point estimates for the
    #'   parameters of interest.
    point_estimate = NULL,

    #' @description Change the value of the `point_estimate` field.
    #'
    #' @param point_estimate A numeric vector providing rough point estimates for the
    #'   parameters under investigation.
    #' @param lower_bound A scalar or numeric vector specifying the lower bounds
    #'   for each parameter under investigation. If it is a scalar, the value is
    #'   used as lower bound for all parameters. Defaults to `-10`.
    #' @param upper_bound A scalar or numeric vector specifying the lower bounds
    #'   for each parameter under investigation. If it is a scalar, the value is
    #'   used as lower bound for all parameters. Defaults to `10`.
    #' @param estimate A boolean specifying whether the rough point estimate
    #'   provided by `val` should serve as initial point for maximizing the
    #'   plausibility function (`estimate = TRUE`) or as final point estimate
    #'   for the parameters (`estimate = FALSE`). Defaults to `FALSE`.
    #' @param overwrite A boolean specifying whether to force the computation if
    #'   it has already been set. Defaults to `FALSE`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$point_estimate
    #' pf$set_point_estimate(mean(y) - mean(x))
    #' pf$point_estimate
    set_point_estimate = function(point_estimate = NULL,
                                  lower_bound = -10,
                                  upper_bound =  10,
                                  estimate = FALSE,
                                  overwrite = FALSE) {
      if (!anyNA(self$point_estimate) && !overwrite) {
        abort("A point estimate has already been set. If you want to compute it again, please re-run the `$set_point_estimate()` method with `overwrite = TRUE)`.")
      }

      if (!is.null(point_estimate) && !rlang::is_named(point_estimate)) {
        cli::cli_alert_warning("The input point estimate vector is not named. The names provided via the `stat_assignments` list will be used instead.")
        names(point_estimate) <- names(self$parameters)
      }

      if (!is.null(point_estimate) && !estimate) {
        self$point_estimate <- point_estimate
      } else {
        if (length(lower_bound) == 1)
          lower_bound <- rep(lower_bound, self$nparams)
        if (length(upper_bound) == 1)
          upper_bound <- rep(upper_bound, self$nparams)
        opt <- compute_point_estimate(
          pf = self,
          guess = point_estimate,
          lower_bound = lower_bound,
          upper_bound = upper_bound
        )
        self$point_estimate <- opt$par
        names(self$point_estimate) <- names(self$parameters)
      }
      self$parameters <- purrr::map2(self$parameters, self$point_estimate, ~ {
        .x$point_estimate <- .y
        .x
      })
      private$set_univariate_nulls()
    },

    #' @field parameters A list of functions of class `param` produced via
    #'   \code{\link[dials]{new_quant_param}} that stores the parameters to be
    #'   inferred along with important properties such as their name, range,
    #'   etc. Defaults to `NULL`.
    parameters = NULL,

    #' @description Change the value of the `parameters` field.
    #'
    #' Updates the range of the parameters under investigation.
    #'
    #' @param point_estimate A numeric vector providing a point estimate for
    #'   each parameter under investigation. If no estimator is known by the
    #'   user, (s)he can resort to the `$set_point_estimate()` method to get a
    #'   point estimate by maximizing the plausibility function.
    #' @param conf_level A numeric value specifying the confidence level to be
    #'   used for setting parameter bounds. It should be in (0,1).
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$set_nperms(50)
    #' pf$set_point_estimate(point_estimate = mean(y) - mean(x))
    #' pf$parameters
    #' pf$set_parameter_bounds(
    #'   point_estimate = pf$point_estimate,
    #'   conf_level = 0.8
    #' )
    #' pf$parameters
    set_parameter_bounds = function(point_estimate, conf_level) {
      if (!any(dials::has_unknowns(self$parameters)) &&
          is_equal(point_estimate, self$point_estimate) &&
          conf_level == self$max_conf_level) {
        abort("Parameter bounds have already been computed for the provided point estimate and confidence level.")
      }

      if (length(point_estimate) != self$nparams)
        abort("The length of the point estimate does not match the number of parameters.")

      if (is.null(self$point_estimate) || !is_equal(point_estimate, self$point_estimate)) {
        cli::cli_alert_info("Setting new point estimate in field `$point_estimate`.")
        self$set_point_estimate(point_estimate)
      }

      if (conf_level != self$max_conf_level) {
        cli::cli_alert_info("Setting new maximum confidence level in field `$max_conf_level`.")
        self$set_max_conf_level(conf_level)
      }

      for (param_index in 1:self$nparams) {
        pvf_temp <- PlausibilityFunction$new(
          null_spec = private$univariate_nulls[[param_index]],
          stat_functions = private$stat_functions,
          stat_assignments = private$stat_assignments[param_index],
          !!!private$data,
          seed = self$seed
        )
        pvf_temp$set_nperms(self$nperms)
        pvf_temp$set_alternative("two_tail")

        pe <- self$point_estimate[param_index]
        conf_level <- 1 - (1 - self$max_conf_level) / self$nparams

        cli::cli_alert_info(paste0(
          "Computing a confidence interval with confidence level ",
          conf_level,
          " for parameter ",
          names(self$parameters)[param_index],
          "..."
        ))

        self$parameters[[param_index]] <- dials::finalize(
          object = self$parameters[[param_index]],
          pf = pvf_temp,
          conf_level = conf_level
        )
      }
    },

    #' @field grid A tibble storing evaluations of the plausibility function on
    #'   a regular centered grid of the parameter space. Defaults to `NULL`.
    grid = NULL,

    #' @description Computes a tibble storing a regular centered grid of the
    #'   parameter space.
    #'
    #' @param parameters A list of \code{\link[dials]{new_quant_param}} objects
    #'   containing information about the parameters under investigation. It
    #'   should contain the fields `point_estimate` and `range`.
    #' @param npoints An integer specifying the number of points to discretize
    #'   each dimension. Defaults to `20L`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$set_nperms(50)
    #' pf$set_point_estimate(mean(y) - mean(x))
    #' pf$set_parameter_bounds(
    #'   point_estimate = pf$point_estimate,
    #'   conf_level = 0.8
    #' )
    #' pf$set_grid(
    #'   parameters = pf$parameters,
    #'   npoints = 2L
    #' )
    set_grid = function(parameters, npoints = 20L) {
      point_estimate <- get_point_estimate(parameters)
      range_list <- get_ranges(parameters)
      if (!is.null(self$grid) &&
          is_equal(self$point_estimate, point_estimate) &&
          equal_ranges(self$parameters, range_list) &&
          npoints == private$npoints) {
        abort("A grid have already been generated for the provided point estimate, ranges and grid size.")
      }

      if (length(point_estimate) != self$nparams)
        abort("The length of the point estimate does not match the number of parameters.")

      if (length(range_list) != self$nparams)
        abort("The length of the parameter list does not match the number of parameters.")

      if (is.null(self$point_estimate) || !is_equal(self$point_estimate, point_estimate)) {
        cli::cli_alert_info("Setting new point estimate in field `$point_estimate`.")
        self$set_point_estimate(point_estimate)
      }

      if (any(dials::has_unknowns(self$parameters)) || !equal_ranges(self$parameters, range_list)) {
        cli::cli_alert_info("Setting new parameter ranges in field `$parameters`.")
        self$parameters <- purrr::map2(self$parameters, range_list, dials::range_set)
        self$parameters <- purrr::map2(self$parameters, self$point_estimate, ~ {
          .x$point_estimate <- .y
          .x
        })
      }

      if (npoints != private$npoints) {
        cli::cli_alert_info("Setting new grid size in field `$npoints`.")
        private$set_npoints(npoints)
      }

      self$grid <- grid_biregular(
        self$parameters,
        center = self$point_estimate,
        levels = private$npoints
      )
    },

    #' @description Updates the `grid` field with a `pvalue` column storing
    #'   evaluations of the plausibility function on the regular centered grid
    #'   of the parameter space.
    #'
    #' @param grid A \code{\link[tibble]{tibble}} storing a grid that spans the
    #'   space of parameters under investigation.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$set_nperms(50)
    #' pf$set_point_estimate(mean(y) - mean(x))
    #' pf$set_parameter_bounds(
    #'   point_estimate = pf$point_estimate,
    #'   conf_level = 0.8
    #' )
    #' pf$set_grid(
    #'   parameters = pf$parameters,
    #'   npoints = 2L
    #' )
    #' pf$evaluate_grid(grid = pf$grid)
    evaluate_grid = function(grid) {
      if ("pvalue" %in% names(self$grid) && is_equal(grid, self$grid)) {
        abort("The current grid has already been evaluated.")
      }

      if (is.null(self$grid) || !is_equal(grid, self$grid)) {
        cli::cli_alert_info("Setting new grid in field `$grid`.")
        self$grid <- grid
      }

      transformed_grid <- self$grid %>%
        purrr::array_tree(margin = 1)

      cli::cli_alert_info("Evaluating grid.")
      p <- progressr::progressor(steps = length(transformed_grid)) #progress bar set up

      self$grid$pvalue <- furrr::future_map_dbl(transformed_grid, function(.l) {
          p() #progress bar update
          self$get_value(.l)
        }, .options = furrr::furrr_options(seed = TRUE))
    },

    #' @field seed A numeric value specifying the seed to be used. Defaults to `1234`.
    seed = 1234
  ),
  private = list(
    null_spec = NULL,
    set_null_spec = function(val) {
      private$null_spec <- rlang::as_function(val)
    },

    data = NULL,
    nsamples = 2,
    set_data = function(...) {
      private$data <- convert_to_list(...)
      private$data[[2]] <- as.factor(private$data[[2]])
      private$nsamples <- length(levels(private$data[[2]]))
    },

    set_nparams = function(val) {
      self$nparams <- val
    },

    stat_functions = NULL,
    set_stat_functions = function(val) {
      private$stat_functions <- purrr::map(val, rlang::as_function)
    },

    alternative_choices = c("two_tail", "left_tail", "right_tail"),
    aggregator_choices = c("tippett", "fisher"),
    pvalue_formula_choices = c("exact", "upper_bound", "estimate"),

    univariate_nulls = NULL,
    set_univariate_nulls = function() {
      private$univariate_nulls <- 1:self$nparams %>%
        purrr::map(~ function(y, parameters) {
          all_parameters <- self$point_estimate
          all_parameters[.x] <- parameters[1]
          private$null_spec(y, all_parameters)
        })
    },

    stat_assignments = NULL,
    set_stat_assignments = function(val) {
      private$stat_assignments <- val
    },

    npoints = 20L,
    set_npoints = function(val) {
      private$npoints <- val
    }
  )
)

compute_point_estimate <- function(pf,
                                   guess = NULL,
                                   lower_bound = -10,
                                   upper_bound =  10,
                                   verbose = FALSE) {
  nparams <- pf$nparams

  if (!is.null(guess)) {
    # if user did not set a default cluster we compute in sequential
    if (is.null(parallel::getDefaultCluster())) {
      cli::cli_alert_warning("Default cluster has not been set. You may consider setting one to run computation in parallel.")
      opt <- stats::optim(
        par = guess,
        fn = pf$get_value,
        method = "L-BFGS-B",
        control = list(fnscale = -1)
      )
    }
    else {
      # uses user default cluster to compute in parallel
      opt <- optimParallel::optimParallel(
        par = guess,
        fn = pf$get_value,
        control = list(fnscale = -1),
        parallel = list(cl = NULL, forward = FALSE, loginfo = FALSE)
      )
    }
    x0 <- opt$par
    fval <- opt$value
  } else {
    if (length(lower_bound) != nparams)
      abort("The number of provided lower bounds does not match the number of parameters.")

    if (length(upper_bound) != nparams)
      abort("The number of provided upper bounds does not match the number of parameters.")

    # if user default cluser is not set, will run in sequential, else run in parallel
    opt <- rgenoud::genoud(
      fn = pf$get_value,
      nvars = nparams,
      Domains = cbind(lower_bound, upper_bound),
      max = TRUE,
      pop.size = 20 * nparams,
      max.generations = 10 * nparams,
      wait.generations = 2 * nparams + 1,
      BFGSburnin = 2 * nparams + 1,
      print.level = 0,
      cluster = if (!is.null(parallel::getDefaultCluster())) parallel::getDefaultCluster() else FALSE,
      balance = nparams > 2
    )
    opt <- compute_point_estimate(
      pf = pf,
      guess = opt$par,
      verbose = FALSE
    )
    x0 <- opt$par
    fval <- opt$value
  }

  if (verbose) {
    cli::cli_alert_info(paste0(
      "Local maximum ",
      round(fval, 3),
      " reached at x0 = ",
      round(x0, 3),
      "."
    ))
  }

  list(par = x0, value = fval)
}

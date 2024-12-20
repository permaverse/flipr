compute_point_estimate <- function(pf,
                                   guess = NULL,
                                   lower_bound = -10,
                                   upper_bound =  10,
                                   verbose = FALSE) {
  nparams <- pf$nparams

  if (!is.null(guess)) {
    #if (ncores == 1) {
      # opt <- stats::optim(
      #   par = guess,
      #   fn = pf$get_value,
      #   method = "L-BFGS-B",
      #   control = list(fnscale = -1)
      # )
    #} else {
      # parallel_opts <- list(
      #   cl = parallel::makeCluster(ncores),
      #   forward = FALSE,
      #   loginfo = FALSE
      # )

      opt <- optimParallel::optimParallel(
        par = guess,
        fn = pf$get_value,
        control = list(fnscale = -1)#,
        #parallel = parallel_opts #here, will use default cluster
      )
      #parallel::stopCluster(parallel_opts$cl)
    #}
    x0 <- opt$par
    fval <- opt$value
  } else {
    if (length(lower_bound) != nparams)
      abort("The number of provided lower bounds does not match the number of parameters.")

    if (length(upper_bound) != nparams)
      abort("The number of provided upper bounds does not match the number of parameters.")

    # if (nparams == 1 && ncores == 1) {
    #   opt <- stats::optimise(
    #     f = pf$get_value,
    #     interval = c(lower_bound, upper_bound),
    #     maximum = TRUE
    #   )
    #   x0 <- opt$maximum
    #   fval <- opt$objective
    # } else {
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
        cluster = parallel::getDefaultCluster(),
        balance = nparams > 2
      )
      opt <- compute_point_estimate(
        pf = pf,
        guess = opt$par,
        verbose = FALSE
      )
      x0 <- opt$par
      fval <- opt$value
    #}
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

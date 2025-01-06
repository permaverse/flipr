#' Visualization of Plausibility Functions
#'
#' This function plots the plausibility function for up to two parameters of
#' interest.
#'
#' @param pf A \code{\link{PlausibilityFunction}} object.
#' @param alpha A numeric value specifying a significance level to contrast the
#'   plausibility function against. Defaults to `0.05`.
#' @param ngrid An integer specifying the grid size on which the plausibility
#'   function will be evaluated. Specifically if `K` is the number of parameters
#'   under investigation, the grid will be of size `(ngrid + 1)^K`. Defaults to
#'   `10L`.
#' @param subtitle A string for specifying a subtitle to the plot. Defaults to
#'   `""` leading to no subtitle.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
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
#' plot_pf(pf)
plot_pf <- function(pf, alpha = 0.05, ngrid = 10, subtitle = "") {
  if (pf$nparams > 2)
    abort("Only one- or two-dimensional plausibility functions can currently be plotted.")

  if (is.null(pf$grid))
    abort("The plausbility function has not yet been evaluated on a grid of parameters. Consider running the `$set_grid()` method first.")

  if (requireNamespace("viridis", quietly = TRUE)) {
    color_palette <- viridis::viridis(3)
  } else if (requireNamespace("viridisLite", quietly = TRUE)) {
    color_palette <- viridisLite::viridis(3)
  } else {
    color_palette <- gg_color_hue(3)
  }

  if (pf$nparams == 1) {
    nm <- names(pf$parameters)
    pf$grid %>%
      ggplot(aes_string(nm[1], "pvalue")) +
      geom_line() +
      labs(
        title = format_title(paste(
          pf$alternative,
          pf$pvalue_formula,
          "p-value function"
        )),
        subtitle = format_title(paste(
          "Using",
          pf$nperms,
          "randomly sampled permutations from seed",
          pf$seed
        )),
        y = "p-value"
      ) +
      theme_bw() +
      scale_y_continuous(limits = c(0, 1)) +
      geom_vline(
        xintercept = pf$point_estimate,
        color = color_palette[3]
      ) +
      geom_vline(
        xintercept = unlist(pf$parameters[[1]]$range),
        color = color_palette[2],
        linetype = "dashed"
      ) +
      geom_hline(
        yintercept = alpha,
        color = color_palette[1],
        linetype = "dashed"
      ) +
      geom_area(
        data = pf$grid %>%
          subset(
            pf$grid[[nm[1]]] >= pf$parameters[[1]]$range$lower &
              pf$grid[[nm[1]]] <= pf$parameters[[1]]$range$upper
          ),
        fill = color_palette[2],
        alpha = 0.2
      ) +
      scale_x_continuous(
        breaks = round(
          x = c(pf$parameters[[1]]$range$lower, pf$point_estimate, pf$parameters[[1]]$range$upper),
          digits = 3
        )
      )
  } else {
    nm <- names(pf$parameters)
    pf$grid %>%
      ggplot(aes_string(nm[1], nm[2], z = "pvalue")) +
      geom_contour_filled(binwidth = 0.05) +
      labs(
        title = "Contour plot of the plausibility function",
        subtitle = subtitle,
        x = pf$parameters[[1]]$label,
        y = pf$parameters[[2]]$label,
        fill = "p-value"
      ) +
      theme_minimal()
  }
}

gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

format_title <- function(x) {
  x <- gsub("_", "-", x)
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

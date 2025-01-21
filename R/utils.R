abort <- function(msg) {
  cli::cli_alert_danger(msg)
  withr::with_options(list(show.error.messages = FALSE), stop())
}

convert_to_list <- function(...) {
  l <- rlang::list2(...)
  n <- length(l)

  # Case "No input samples"
  if (n == 0) return(NULL)

  # Case of distance matrix
  if (inherits(l[[1]], "dist")) {
    if (n == 1) return(l)
    coherent_inputs <- TRUE
    for (i in 2:n) {
      if (!is.integer(l[[i]])) {
        coherent_inputs <- FALSE
        break
      }
    }
    stopifnot(coherent_inputs)
    return(l)
  }

  # Case of anova: M variables + a factor
  if (n == 2 && is.factor(l[[2]])) {
    vars <- l[[1]]
    coherent_inputs <- TRUE
    for (i in 1:length(vars)) {
      if (!is.matrix(vars[[i]]) && !rlang::is_bare_numeric(vars[[i]])) {
        coherent_inputs <- FALSE
        break
      }
    }
    stopifnot(coherent_inputs)
    return(l)
  }

  # Case of univariate data
  if (rlang::is_bare_numeric(l[[1]])) {
    if (n > 1) {
      coherent_inputs <- TRUE
      for (i in 2:n) {
        if (!rlang::is_bare_numeric(l[[i]])) {
          coherent_inputs <- FALSE
          break
        }
      }
      stopifnot(coherent_inputs)
    }
    return(purrr::map(l, purrr::array_tree, margin = 1))
  }

  # Case of multivariate data
  if (is.matrix(l[[1]])) {
    if (n > 1) {
      coherent_inputs <- TRUE
      for (i in 2:n) {
        if (!is.matrix(l[[i]]) || (ncol(l[[i]]) != ncol(l[[1]]))) {
          coherent_inputs <- FALSE
          break
        }
      }
      stopifnot(coherent_inputs)
    }
    return(purrr::map(l, purrr::array_tree, margin = 1))
  }

  coherent_inputs <- TRUE
  for (i in 1:n) {
    if (!is.list(l[[i]])) {
      coherent_inputs <- FALSE
      break
    }
  }
  stopifnot(coherent_inputs)

  l
}

get_ranges <- function(parameters) {
  purrr::map(parameters, list(dials::range_get, unlist, as.numeric))
}

equal_ranges <- function(parameters, range_list) {
  is_equal(get_ranges(parameters), range_list)
}

is_equal <- function(x, y) {
  isTRUE(all.equal(x, y))
}

format_param_label <- function(x) {
  x <- gsub("[_-]", " ", x)
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
}

get_point_estimate <- function(params) {
  point_estimate <- purrr::map(params, "point_estimate")
  is_ukn <- purrr::map_lgl(point_estimate, dials::is_unknown)
  point_estimate[is_ukn] <- NA
  unlist(point_estimate)
}

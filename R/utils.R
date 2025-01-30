abort <- function(msg) {
  cli::cli_alert_danger(msg)
  withr::with_options(list(show.error.messages = FALSE), stop())
}

# convert data to flipr format
# for distance matrix:
# returns list(distance matrix, group memberships)
# for other data types:
# returns list(list(vars), group memberships) for 1-sample, 2-sample or ANOVA
# returns list(list(response var), list(qualitative vars) , list(other vars)) for regression
convert_to_list <- function(..., flag_anova = FALSE) {
  l <- rlang::list2(...)
  n <- length(l)

  # Case "No input samples"
  if (n == 0) return(NULL)

  # Case of already in the flipr format
  if (is_flipr_format(...)) {
    return(l)
  }

  # Case of (M)ANOVA with a factor as second argument
  if (n == 2 && is.factor(l[[2]])) {
    if (is.list(l[[1]][[1]])) {
      new_data <- as.data.frame(l[[1]])
      colnames(new_data) <- NULL
      return(list(convert_to_list(list(new_data), flag_anova = TRUE)[[1]], l[[2]]))
    }
    return(list(convert_to_list(l[[1]], flag_anova = TRUE), l[[2]]))
  }

  # Case of distance matrix
  if (inherits(l[[1]], "dist")) {
    if (n != 2 || (!is.factor(l[[2]]) && !is.integer(l[[2]]))) {
      abort("When using distance matrix, user should pass to flipr one distance matrix with all observations
            and a vector indicating class memberships.")
    }
    coherent_inputs <- TRUE
    return(list(l[[1]], as.factor(l[[2]])))
  }

  # Case of univariate data
  if (rlang::is_bare_numeric(l[[1]]) && !is.matrix(l[[1]]) && !is.data.frame(l[[1]])) {
    new_factor <- rep(1, length(l[[1]]))
    if (n == 1) {
      if (flag_anova) return(as.list(l[[1]]))
      return(list(as.list(l[[1]]), new_factor))
    }
    if (n > 1) {
      coherent_inputs <- TRUE
      new_data <- as.list(l[[1]])
      for (i in 2:n) {
        new_factor <- c(new_factor, rep(i, length(l[[i]])))
        new_data <- c(new_data, l[[i]])
        if (!rlang::is_bare_numeric(l[[i]])) {
          coherent_inputs <- FALSE
          break
        }
      }
      stopifnot(coherent_inputs)
    }
    if (flag_anova) return(new_data)
    return(list(new_data, new_factor))
  }

  # Case of multivariate data
  if (is.matrix(l[[1]])) {
    new_factor <- rep(1, nrow(l[[1]]))
    new_data <- purrr::map(list(l[[1]]), purrr::array_tree, margin = 1)
    if (flag_anova) return(new_data)
    if (n > 1) {
      coherent_inputs <- TRUE
      for (i in 2:n) {
        new_factor <- c(new_factor, rep(i, nrow(l[[i]])))
        new_data <- list(c(new_data[[1]], purrr::map(list(l[[i]]), purrr::array_tree, margin = 1)[[1]]))
        if (!is.matrix(l[[i]]) || (ncol(l[[i]]) != ncol(l[[1]]))) {
          coherent_inputs <- FALSE
          break
        }
      }
      stopifnot(coherent_inputs)
    }
    return(c(new_data, list(new_factor)))
  }
  if (is.data.frame(l[[1]])) {
    new_factor <- rep(1, nrow(l[[1]]))
    new_data <- purrr::map(list(l[[1]]), purrr::array_tree, margin = 1)
    if (flag_anova) return(new_data)
    if (n > 1) {
      coherent_inputs <- TRUE
      for (i in 2:n) {
        new_factor <- c(new_factor, rep(i, nrow(l[[i]])))
        new_data <- list(c(new_data[[1]], purrr::map(list(l[[i]]), purrr::array_tree, margin = 1)[[1]]))
        if (!is.data.frame(l[[i]]) || (ncol(l[[i]]) != ncol(l[[1]]))) {
          coherent_inputs <- FALSE
          break
        }
      }
      stopifnot(coherent_inputs)
    }
    return(c(new_data, list(new_factor)))
  }

  # TODO Case regression

  # Case of other objects contained in lists
  if (is.list(l[[1]])) {
    new_factor <- rep(1, length(l[[1]]))
    if (n == 1) {
      return(list(l[[1]], new_factor))
    }
    if (n > 1) {
      coherent_inputs <- TRUE
      new_data <- l[[1]]
      for (i in 2:n) {
        new_factor <- c(new_factor, rep(i, length(l[[i]])))
        new_data <- c(new_data, l[[i]])
        if (!is.list(l[[i]])) {
          coherent_inputs <- FALSE
          break
        }
      }
      stopifnot(coherent_inputs)
    }
    return(list(new_data, new_factor))
  }


  for (i in 1:n) {
    if (!is.list(l[[i]])) {
      coherent_inputs <- FALSE
      break
    }
  }
  stopifnot(coherent_inputs)

  l
}

is_flipr_format <- function(...) {
  l <- rlang::list2(...)
  n <- length(l)
  # data format for k-sample tests
  if(n == 2 && is.factor(l[[2]]) && is.list(l[[1]]) && length(l[[2]]) == length(l[[1]])) {
    return(TRUE)
  # data format for distance matrix
  } else if (n == 2 && is.factor(l[[2]]) && inherits(l[[1]], "dist")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  # TODO for regression
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

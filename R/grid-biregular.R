#' Create a biregular grid around a center point
#'
#' Biregular grids can be created for any number of parameter objects.
#'
#' @inherit dials::grid_regular
#' @param center A numeric vector specifying the point onto which the biregular
#'   grid should be centered. Defaults to `NULL`, in which case
#'   \code{\link[dials]{grid_regular}} is used instead.
#' @export
#'
#' @examples
#' grid_biregular(dials::mixture(), center = 0.2)
grid_biregular <- function(x, ..., center = NULL, levels = 3, original = TRUE, filter = NULL) {
  if (is.null(center))
    return(dials::grid_regular(x, ..., levels = levels, original = original, filter = filter))

  if (levels %% 2 != 0) levels <- levels - 1

  dots <- list(...)

  if (any(names(dots) == "size")) {
    rlang::warn("`size` is not an argument to `grid_biregular()`. Did you mean `levels`?")
  }

  .grid_biregular(x, ..., center = center, levels = levels, original = original, filter = filter)
}

.grid_biregular <- function(x, ..., center = NULL, levels = 3, original = TRUE, filter = NULL) {
  UseMethod(".grid_biregular")
}

.grid_biregular.parameters <- function(x, ..., center, levels = 3, original = TRUE, filter = NULL) {
  params <- x$object
  if (length(center) != length(params))
    abort("The length of the center point does not match the number of parameters.")
  names(params) <- x$id
  make_biregular_grid(params, center, levels = levels, original = original)
}

.grid_biregular.list <- function(x, ..., center, levels = 3, original = TRUE, filter = NULL) {
  y <- dials::parameters(x)
  params <- y$object
  if (length(center) != length(params))
    abort("The length of the center point does not match the number of parameters.")
  names(params) <- y$id
  make_biregular_grid(params, center, levels = levels, original = original)
}

.grid_biregular.param <- function(x, ..., center, levels = 3, original = TRUE, filter = NULL) {
  y <- dials::parameters(list(x, ...))
  params <- y$object
  if (length(center) != length(params))
    abort("The length of the center point does not match the number of parameters.")
  names(params) <- y$id
  make_biregular_grid(params, center, levels = levels, original = original)
}

.grid_biregular.workflow <- function(x, ..., center, levels = 3, original = TRUE, filter = NULL) {
  .grid_biregular.parameters(
    dials::parameters(x),
    ...,
    center = center,
    levels = levels,
    original = original,
    filter = {{filter}}
  )
}

make_biregular_grid <- function(params, center, levels = 3, original = TRUE) {
  params %>%
    purrr::map2(center, ~ {
      center_value <- .y
      rngs <- dials::range_get(.x, original = original)
      min_value <- rngs$lower
      max_value <- rngs$upper
      stopifnot(center_value > min_value && center_value < max_value)
      c(
        seq(min_value, center_value, len = levels / 2 + 1)[1:(levels / 2)],
        center_value,
        seq(center_value, max_value, len = levels / 2 + 1)[-1]
      )
    }) %>%
    c(list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)) %>%
    do.call(expand.grid, .) %>%
    tibble::as_tibble()
}

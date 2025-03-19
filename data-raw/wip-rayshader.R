pf <- readRDS("~/Downloads/pf-1anova.rds")

upsample_grid <- function(grid, n = 100L) {
  nms <- colnames(grid)
  pval_mat <- tidyr::pivot_wider(grid, names_from = nms[2], values_from = pvalue)
  pval_mat <- as.matrix(pval_mat[, -1])
  old_x <- sort(unique(pf$grid[[nms[1]]]))
  old_y <- sort(unique(pf$grid[[nms[2]]]))
  itp <- interp::bilinear.grid(old_x, old_y, pval_mat, nx = n, ny = n)
  out <- tidyr::crossing(x = itp$x, y = itp$y)
  dplyr::mutate(out, pvalue = c(itp$z))
}

plot_pvalue_surface <- function(pf, n = 100L) {
  p1 <- pf$grid |>
    upsample_grid(n = n) |>
    ggplot(aes(x, y, z = pvalue)) +
    geom_contour_filled(binwidth = 0.05) +
    geom_contour(color = "black", binwidth = 0.05) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed() +
    theme(legend.position = "none") +
    labs(
      title = "Contour plot of the p-value surface",
      subtitle = "Using Fisher's non-parametric combination",
      x = expression(mu[2] - mu[1]),
      y = expression(mu[3] - mu[1]),
      fill = "p-value"
    )

  if (!requireNamespace("rayshader", quietly = TRUE)) {
    return(p1)
  }

  p2 <- pf$grid |>
    upsample_grid(n = n) |>
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = pvalue)) +
    geom_contour(aes(z = pvalue), color = "black", binwidth = 0.05) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed() +
    scale_fill_gradientn(colours = terrain.colors(10)) +
    theme(legend.position = "none") +
    labs(
      title = "Contour plot of the p-value surface",
      subtitle = "Using Fisher's non-parametric combination",
      x = expression(mu[2] - mu[1]),
      y = expression(mu[3] - mu[1]),
      fill = "p-value"
    )

  rayshader::plot_gg(
    p1, ggobj_height = p2,
    multicore = TRUE, raytrace = TRUE, width = 7, height = 4,
    scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30
  )
}

plot_pvalue_surface(pf, n = 50L)

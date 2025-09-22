#' Fibonacci Sphere
#'
#' @param samples Number of points to generate on the sphere.
#'
#' @returns A matrix of points on the sphere, where each row is a point in 3D space.
#'
#' @examples
#' points <- fibonacci_sphere()
#' if(requireNamespace("plotly", quietly = TRUE)){
#'   plotly::plot_ly(x = points[, 1], y = points[, 2], z = points[, 3],
#'     type = "scatter3d", mode = "markers", marker = list(size = 5))
#' }
#'
#' @export
fibonacci_sphere <- function(samples = 1000) {
  phi <- pi * (sqrt(5) - 1)  # gyldent snit i radianer
  y <- seq(-1, 1, length.out = samples)
  radius <- sqrt(1 - y * y)  # radius ved y
  theta <- phi * (0:(samples-1))  # gyldent snit step
  x <- cos(theta) * radius
  z <- sin(theta) * radius
  points <- cbind(x, y, z)
  return(points)
}


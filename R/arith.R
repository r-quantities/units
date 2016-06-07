
#' Title add two vectors with identical units
#'
#' @param x first vector, of class \code{units}
#' @param y second vector, of class \code{units}
#'
#' @return numeric vector of class \code{units}
#' @export 
#'
#' @examples
#' a = 1:3
#' b = 7:9
#' plus(a,b)
plus = function(x, y) { x + y }
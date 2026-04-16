#' @title Triangular Distribution
#' @description Density, distribution function, quantile function and random generation
#' for the triangular distribution.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of observations.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @name triangular
#'
#'
#' @rdname triangular
#' @export
#' dtriang <- function(x, min, max, mode) {
if (any(min > max)) stop("min cannot be greater than max")
if (any(mode < min | mode > max)) stop("mode must be between min and max")


}

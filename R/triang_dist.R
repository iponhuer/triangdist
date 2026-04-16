#' @title Triangular Distribution
#' @description Density, distribution function, quantile function and
#' random generation for the triangular distribution.
#'
#' @importFrom stats runif
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
dtriang <- function(x, min, max, mode) {
  if (any(min > max)) stop("min cannot be greater than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")

  res <- numeric(length(x))

  #Upward slope, between min and mode
  up <- x >= min & x < mode
  res[up] <- (2 * (x[up] - min)) / ((max - min) * (mode - min))

  #At the mode
  #Assigns the value of the maximum height only when x is equal to the mode
  #The sum must be 1
  res[x == mode] <- 2 / (max - min)

  #Downward slope, between mode and max
  down <- x > mode & x <= max
  res[down] <- (2 * (max - x[down])) / ((max - min) * (max - mode))

  res

}


#' @rdname triangular
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max)) stop("min cannot be greater than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")

  res <- numeric(length(q))
  res[q > max] <- 1 #if it is bigger than max means that cdf is 1

  izq <- q >= min & q <= mode
  der <- q > mode & q <= max

  res[izq] <- (q[izq] - min) ^ 2 / ((max - min) * (mode - min))
  res[der] <- 1 - (max - q[der]) ^ 2 / ((max - min) * (max - mode))

  res

}

#' @rdname triangular
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(min > max)) stop("min cannot be greater than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")

  res <- numeric(length(p))
  p_crit <- (mode - min) / (max - min) #cdf at the mode

  lower <- p <= p_crit
  upper <- p > p_crit

  res[lower] <- min + sqrt(p[lower] * (max - min) * (mode - min))
  res[upper] <- max - sqrt((1 - p[upper]) * (max - min) * (max - mode))

  res

}

#' @rdname triangular
#' @export
rtriang <- function(n, min, max, mode) {
  qtriang(runif(n), min, max, mode)
}

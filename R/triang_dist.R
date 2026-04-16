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

return(res)

}


#' @rdname triangular
#' @export
ptriang <- function(x, min, max, mode) {
  if (any(min > max)) stop("min cannot be greater than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")

}





#' @rdname triangular
#' @export
qtriang <- function(x, min, max, mode) {
  if (any(min > max)) stop("min cannot be greater than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")
  if (any(x < 0 | x > 1)) stop("x must be between 0 and 1")



}




#' @rdname triangular
#' @export
rtriang <- function(n, min, max, mode){

}

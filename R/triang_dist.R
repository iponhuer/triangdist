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
dtriang <- function(x, min, max, mode){
if (any(min > max)) stop("min cannot be greater than max")
if (any(mode < min | mode > max)) stop("mode must be between min and max")

res <- numeric(length(x))

#Upward slope, between min and mode
up <- x >= min & x < mode
res[up] <- (2*(x[up] - min)) / ((max-min)*(mode-min))

#At the mode
#Assigns the value of the maximum height only when x is equal to the mode
#The sum must be 1
res[x == mode] <- 2 /(max - min)

#Downward slope, between mode and max
down <- x > mode & x <= max
res[down] <- (2*(max - x[down])) / ((max-min) * (max-mode))

return(res)

}


#' @rdname triangular
#' @export
ptriang <- function(x, min, max, mode) {
  if (any(min > max)) stop("min cannot be greater than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")

  res <- numeric(length(x))
  res[x > max] <- 1 #if it is bigger than max means that cdf is 1

  izq <- x >= min & x <= mode
  der <- x > mode & x <= max

  res[izq] <- (x[izq] - min)^2 / ((max-min) * (mode-min))
  res[der] <- 1 - (max - x[der])^2 / ((max-min) * (max-mode))

  return(res)

}

#' @rdname triangular
#' @export
qtriang <- function(x, min, max, mode) {
  if (any(min > max)) stop("min cannot be greater than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")
  if (any(x < 0 | x > 1)) stop("x must be between 0 and 1")

  res <- numeric(length(x))
  p_crit <- (mode-min) / (max-min) #cdf at the mode

  #If the probability is less than p_crit the result will be in the first half of the triangle
  #This is the one that goes up

  #If it is higher the result will be in the second half
  #This is the one that goes down

  lower <- p <= p_crit
  upper <- p > p_crit

  res[lower] <- min + sqrt(p[lower] * (max - min) * (mode - min))
  res[upper] <- max - sqrt((1 - p[upper]) * (max - min) * (max - mode))

  return(res)

}




#' @rdname triangular
#' @export
rtriang <- function(n, min, max, mode){

}

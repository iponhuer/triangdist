# Triangdist

This is an R package designed to implement the Triangular Distribution. 
The package includes functions for calculating the density, the cumulative distribution, 
quantiles, and the generation of random numbers.


## For installing the package from GitHub:

remotes::install_github("iponhuer/triangdist")

## Main Functions
The package implements the triangular distribution defined by three parameters: 
a lower bound (a), an upper bound (b), and a mode (c). 
The included functions are:


dtriang(x, min, max, mode): Probability density function.

ptriang(q, min, max, mode): Cumulative distribution function.

qtriang(p, min, max, mode): Quantile function.

rtriang(n, min, max, mode): Random number generation based on the inversion method.

## Examples of using:

library(triangdist)

Calculate the density at x = 0.5 for a distribution on [0, 1] with mode 0.5
dtriang(0.5, min = 0, max = 1, mode = 0.5)

Generate 10 random numbers
rtriang(10, min = 0, max = 1, mode = 0.5)

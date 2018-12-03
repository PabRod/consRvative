## Clean environment
rm(list = ls())
library(numDeriv)
library(Matrix)

## Input function
# Example of a flow R^n --> R^n
flow <- function(x) {
  c(
    -x[1] - sin(x[2]),
    x[2],
    x[3]
  )
}

# The jacobian is R^n --> R^n x R^n
aux <- function(x) {
  #do.call(jacobian, c(flow, as.numeric(x)))
  jacobian(flow, as.numeric(x))
}

## Create meshgrid
xs <- seq(-1, 2, length = 2)
ys <- seq(-1, 1, length = 3)
zs <- seq(0, 1, length= 4)

# This is just an ordered list of all the sampling points
rs <- expand.grid(x = xs, y = ys, z = zs) # Cartesian product

rs_list <- split(rs, f = seq(1:nrow(rs))) # listify for lapply

# Extract jacobians
Js <- lapply(rs_list, aux)

# Extract eigenvalues
egv <- lapply(Js, eigen, only.values = FALSE)
lyaps <- lapply(egv, '[[', 'values')
maxLyap <- lapply(lyaps, max)
#lapply(egv, function(x) x$values)

rs$Js <- Js
rs$lyaps <- lyaps
rs$maxLyap <- maxLyap

Js <- matrix(Js, nrow = length(xs), ncol = length(ys), byrow = TRUE) # reshape
lyaps <- matrix(lyaps, nrow = length(xs), ncol = length(ys), byrow = TRUE)

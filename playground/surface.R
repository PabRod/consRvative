## Clean environment
rm(list = ls())

## Load libraries
library(plotly)

## Input function
f <- function(x,y) x^2 + 0.1*y

## Create meshgrid
xs <- seq(-2,2, length=50)
ys <- seq(-1,1, length=30)
zs <- outer(xs, ys, f)



## Plot
contour(xs, ys, zs, nlevels = 20) # Contour understands rows as xs

p <- plot_ly(x = xs,
             y = ys,
             z = t(zs), # Plotly understands rows as ys
             type = 'contour')
print(p)


## Clean environment
rm(list = ls())

## Input vectorized function
f <- function(x) x[1]^2 + 0.1*x[2]

## Create meshgrid
xs <- seq(-2,2, length=5)
ys <- seq(-1,1, length=3)
rs <- expand.grid(x = xs, y = ys) # Cartesian product

zs <- apply(rs, 1, f) # Evaluate
zs <- matrix(zs, nrow = length(xs), ncol = length(ys)) # Reshape

## Plot
contour(xs, ys, zs, nlevels = 20) # Contour understands rows as xs

p <- plot_ly(x = xs,
             y = ys,
             z = t(zs), # Plotly understands rows as ys
             type = 'contour')
print(p)


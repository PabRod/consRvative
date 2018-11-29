## Clean environment
rm(list = ls())

## Load libraries
library(numDeriv)
library(Matrix)
library(dplyr)
library(ggplot2)
library(gridExtra)

## Input
# Example of a flow R^n --> R^n
flow <- function(x) {
  c(
    x[2]^2 - cos(x[1]*x[2]),
    2*x[1]*x[2]
  )
}

## Create meshgrid
xs <- seq(-1, 2, length = 30)
ys <- seq(-1, 1, length = 40)

# This is just an ordered list of all the sampling points
rs <- expand.grid(x = xs, y = ys) # Cartesian product
rs_as_list <- split(rs, seq(1, nrow(rs)))

# The jacobian is a function f: R^n --> R^n x R^n
f <- function(...) {
  # transformation for a single point
  transformation <- function(...) {
    args <- c(...)
    matrix(
      jacobian(flow, args),
      nrow = length(args),
      byrow = TRUE
      )
  }

  # vectorisation
  do.call(mapply, c(transformation, list(...), SIMPLIFY=FALSE))
}

df <- data.frame(r = I(rs_as_list))
df$rs <- rs
df$flow <- lapply(df$r, flow)
df$flow_x <- lapply(df$flow, '[[', 1) %>% unlist
df$flow_y <- lapply(df$flow, '[[', 2) %>% unlist
df$J <- do.call(f, rs)
df$egv <- lapply(df$J, eigen)
df$stabilityLyap <- lapply(df$egv, '[[', 'values') %>% # Drop eigenvectors
              lapply(Re) %>% # Calculate the real part
              lapply(max) %>% # Keep only the maximum
              unlist # And output as numeric
df$Jsymm <- lapply(df$J, symmpart)
df$Jskew <- lapply(df$J, skewpart)
df$symmnorm <- lapply(df$Jsymm, norm, type = "2") %>% unlist
df$skewnorm <- lapply(df$Jskew, norm, type = "2") %>% unlist

tol <- 1e-7
df$cons <-  lapply(df$Jskew, function(x) {all(abs(x) < tol)}) %>% unlist

clean <- select(df, -J, -Jsymm, -Jskew, -egv)

## Plot
p_symm <- (ggplot(data=clean, aes(x=rs$x, y=rs$y, z=symmnorm))
           + stat_contour(geom = "polygon", aes(fill = symmnorm))
           + geom_tile(aes(fill = symmnorm))
           + stat_contour(bins = 15)
           + guides(fill = guide_colorbar(title = "Norm"))
           + ggtitle("Symm part")
)

p_skew <- (ggplot(data=clean, aes(x=rs$x, y=rs$y, z=skewnorm))
  + stat_contour(geom = "polygon", aes(fill = skewnorm))
  + geom_tile(aes(fill = skewnorm))
  + stat_contour(bins = 15)
  + guides(fill = guide_colorbar(title = "Norm"))
  + ggtitle("Skew part")
  )

grid.arrange(p_symm, p_skew, nrow = 1)

p_fx <- (ggplot(data=clean, aes(x=rs$x, y=rs$y, z=flow_x))
           + stat_contour(geom = "polygon", aes(fill = flow_x))
           + geom_tile(aes(fill = flow_x))
           + stat_contour(bins = 15)
           + guides(fill = guide_colorbar(title = "Norm"))
           + ggtitle("f_x")
)

p_fy <- (ggplot(data=clean, aes(x=rs$x, y=rs$y, z=flow_y))
           + stat_contour(geom = "polygon", aes(fill = flow_y))
           + geom_tile(aes(fill = flow_y))
           + stat_contour(bins = 15)
           + guides(fill = guide_colorbar(title = "Norm"))
           + ggtitle("f_y")
)

grid.arrange(p_fx, p_fy, p_symm, p_skew, nrow = 2)


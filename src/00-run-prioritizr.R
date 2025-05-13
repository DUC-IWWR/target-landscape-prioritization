####### Script Information ########################
# Brandon P.M. Edwards
# Target Landscape Prioritization
# 00-run-prioritizr.R
# Created May 2025
# Last Updated May 2025

####### Import Libraries and External Files #######

library(prioritizr)
library(terra)

####### Read Data #################################

tl <- vect("data/raw/tl-2025/TL_0.76.shp")
divers <- rast("data/raw/rasters/divers.tif")
dabblers <- rast("data/raw/rasters/dabblers.tif")
redh <- rast("data/raw/rasters/redh.tif")
canv <- rast("data/raw/rasters/canv.tif")

####### Main Code  ################################

#' First create a raster of locked out areas. That is, anything outside
#' of a target landscape will be locked out and not available for selection.
tl_rast <- rasterize(tl, 
                     rast(ext(divers), resolution=res(divers), crs = crs(divers))) |>
  subst(from = 1, to = 0, others = 1)

pu <- rast(divers, vals = 0.01)
divers <- divers / 1000
dabblers <- dabblers / 1000
redh <- redh / 1000
canv <- canv / 1000

p1 <- problem(pu, c(redh, canv)) %>%
  add_locked_out_constraints(tl_rast) %>%
  add_relative_targets(c(0.55,0.59)) %>%
  add_min_shortfall_objective(budget = 1000) %>%
  add_gurobi_solver()

s1 <- solve(p1, force = TRUE)

####### Output ####################################
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

pu <- rast(divers, vals = 1/1000)
divers <- divers / 1000
dabblers <- dabblers / 1000
redh <- redh / 1000
canv <- canv / 1000

top_budget <- freq(tl_rast, value = 0)$count / 1000

budgets <- seq(9.7915, top_budget, length.out = 100)

portfolio <- vector(mode = "list", length = length(budgets))

for (i in 1:length(budgets))
{
  print(i)
  prob <- problem(pu, c(redh, canv)) %>%
    add_locked_out_constraints(tl_rast) %>%
    add_relative_targets(c(0.55,0.59)) %>%
    add_min_shortfall_objective(budget = budgets[i]) %>%
    add_gurobi_solver()
  
  portfolio[[i]] <- solve(prob, force = TRUE)
}

portfolio_rast <- rast(portfolio)

priority <- sum(portfolio_rast)

####### Output ####################################
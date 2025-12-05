# global.R

library(shiny)
library(ggplot2)
library(dplyr)

# Set this to TRUE when debugging on the server
DIAGNOSTICS <- FALSE

#on the server, point to the up to date libraries
shared_lib <- "/home/elac/shiny_public/libraries_4.4"
if (dir.exists(shared_lib)) {
  .libPaths(c(shared_lib, .libPaths()))
}

# Source the hemodynamic model (defines compute_hemo, baseline_hemo, Pv, baselinePa, dref)
source("model.R")
source("constraints.R")



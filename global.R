# global.R

library(shiny)
library(ggplot2)
library(dplyr)

# Set this to TRUE when debugging on the server
DIAGNOSTICS <- TRUE
# Source the hemodynamic model (defines compute_hemo, baseline_hemo, Pv, baselinePa, dref)
source("model.R")


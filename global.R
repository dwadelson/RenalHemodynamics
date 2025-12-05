# global.R

library(shiny)
library(ggplot2)
library(dplyr)

# Source the hemodynamic model (defines compute_hemo, baseline_hemo, Pv, baselinePa, dref)
source("model.R")
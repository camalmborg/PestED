### Script for running multiple simulations of defense SEM

source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Setting up runs with different parameter values in hierarchy of need:
model_runs <- data.frame(
  Runs = c(),
  Allocation = c(),
  Turnover = c(),
  Efficiency = c()
)

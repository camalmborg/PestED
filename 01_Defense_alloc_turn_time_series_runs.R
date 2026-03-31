### Defense Allocation and Turnover analyses for time series comparisons ###

## Load libraries
library(dplyr)

## Load functions
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Set up runs
# make data frame for each run:
alloc_turn_runs <- data.frame(model_run = 1:9,
                              alloc = alloc_runs,
                              turn = turn_runs,
                              ax_or = c("ax", "ax", "ax", "or", "or", "ctr", "or", "ax", "ax"))

# years for time series:
years = 5


## Setting up model runs
# call the run:
task_id <- as.numeric(Sys.getenv("SGE_TASK_ID"))

# set param values:
defense_alloc_percent = alloc_turn_runs$alloc[task_id]   # allocation percent for whole year
defense_turnover_percent = alloc_turn_runs$turn[task_id]  # turnover percent per day
params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
params$defenseEfficiency = 1


## Running the model
# run SEM with no defense efficiency:
defol_model_run = iterate.SEM(c(0,0,1,1,0), years = years)

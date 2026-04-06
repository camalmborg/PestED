### Defense Allocation and Turnover analyses for time series comparisons ###

# 1-time 100% defoliation with varying allocation/turnover orthogonal and on-axis of param surface

## Load libraries
library(dplyr)

## Load functions
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Set up runs
# make data frame for each run:
alloc_turn_runs <- data.frame(model_run = 1:length(alloc_runs),
                              alloc = alloc_runs,
                              turn = turn_runs,
                              ax_or = c("ax", "or", "ax", "ctr", "or", "ax", "ax"))


## Running the model and collecting results
# make a list for storing time series:
alloc_turn_results <- list()
# years for time series:
years = 5
# when defoliation takes place (1-time defol annual):
defol_days <- c(7000)

# loop:
for (i in 1:nrow(alloc_turn_runs)){
  # set param values:
  defense_alloc_percent = alloc_turn_runs$alloc[i]   # allocation percent for whole year
  defense_turnover_percent = alloc_turn_runs$turn[i]  # turnover percent per day
  params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
  params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
  params$defenseEfficiency = 1
  
  # run the model:
  defol_model_run <- iterate.SEM(c(0,0,1,1,0), t.start = defol_days, years = years)
  
  # save it:
  name <- paste0(i, "_alloc_", defense_alloc_percent, 
                 "_turnover_", defense_turnover_percent,
                 "_postition_", alloc_turn_runs$ax_or[i])
  alloc_turn_results[[name]] <- defol_model_run
  rm(defol_model_run)
}

## Run a defoliation with no defense added cases for each:
# set all param values to 0:
params$defenseAlloc = 0
params$defenseBreakdown = 0
params$defenseEfficiency = 0
# set defense state variable to 0:
X[8] = 0
# run SEM no defense allocation:
defol_no_def = iterate.SEM(c(0,0,1,1,0), t.start = defol_days, years = years)
# add it to the results plot:
name <- paste0(length(alloc_turn_results)+1, "_alloc_0",
               "_turnover_0",
               "_position_NA")
alloc_turn_results[[name]] <- defol_no_def
rm(name)


### Archive ###
## Setting up model runs for parallel jobs
# call the run:
#task_id <- as.numeric(Sys.getenv("SGE_TASK_ID"))


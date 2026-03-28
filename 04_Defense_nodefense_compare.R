### Script for running multiple runs and comparing against no-defense case ###

## Load libraries
library(dplyr)


## Load functions
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")


## Run a default with no defense added case
# set all param values to 0:
params$defenseAlloc = 0
params$defenseBreakdown = 0
params$defenseEfficiency = 0
# run SEM no defense allocation:
default_no_defense = iterate.SEM(c(0,0,0,1,0), years = 3)

## Run default with defense allocation but no defense efficiency
# set param values:
defense_alloc_percent = 70   # allocation percent for whole year
defense_turnover_percent = 0.75  # turnover percent per day
params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
params$defenseEfficiency = 0
# run SEM with no defense efficiency:
default_with_defense = iterate.SEM(c(0,0,0,1,0), years = 3)


## Runs with defense allocation and varying efficiency values:
# set param values:
defense_alloc_percent = 70   # allocation percent for whole year
defense_turnover_percent = 0.75  # turnover percent per day
params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
params$defenseEfficiency = 1
# run SEM with defense and defoliation:
defol <- iterate.SEM(c(0,0,1,1,0), years = 3)

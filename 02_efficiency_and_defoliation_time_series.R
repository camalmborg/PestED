### Defense Efficiency and Defoliation time series ###

# (1) two low-level defoliation cases (5%, 10% defoliation annually)
# (2) one-time 100% defoliation case 

## Load libraries
library(dplyr)

## Load functions
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Running the model and collecting results
# make lists for storing time series results:
defol_def_int_results_5pc <- list()    # 5% annual defoliation case
defol_def_int_results_10pc <- list()    # 10% annual defoliation case

# years for time series:
years = 5
# defoliation intensity:
defol_intensity = c(0.5, 0.1, 1)
# when defoliation takes place (1-time defol annual):
defol_days <- c(7000, 24520, 42040, 59560, 77080)



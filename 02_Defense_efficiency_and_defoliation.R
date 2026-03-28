### Script for testing defense efficiency and defoliation balance parameters

## libraries:
library(dplyr)

## load in functions:
wd <- setwd("/projectnb/dietzelab/malmborg/PestDefense/")
source(paste0(wd, "/00_PestED_Defoliation.R"))

## add in allocation and turnover (set values):
defense_alloc_percent = 70   # allocation percent for whole year
defense_turnover_percent = 0.75  # turnover percent per day
params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep

## Making a ranges of allocation and turnover values (based on literature):
# quantiles:
quants <- seq(0, 1, by = 0.05)
# testing values based on quantiles
defense_efficiencies <- quants    # defense efficiency
defoliation <- quants           # defoliation

## Outputs:
default_result <- list()
defol_result <- list()

## Looping over all values:
# defense efficiency loop:
for (i in 1:length(defense_efficiencies)){
  # set parameter value:
  params$defenseEfficiency = defense_efficiencies[i]
  
  # matrices of results:
  output_default <- matrix(NA, nrow = length(quants), ncol = 13)
  output_defol <- matrix(NA, nrow = length(quants), ncol = 13)
  colnames(output_default) <- varnames
  colnames(output_defol) <- varnames
  
  # defoliation magnitude loop:
  for (j in 1:length(defoliation)){
    # run default (no defoliation case):
    default <- iterate.SEM(c(0,0,0,1,0), years = 4)
    defol <- iterate.SEM(c(0,0,defoliation[j],1,0), years = 4)
    
    # get the output summaries:
    output_default[j,] <- 1-apply(default,2,min)/apply(default,2,max)
    output_defol[j,] <- 1-apply(defol,2,min)/apply(defol,2,max)
  }
  # all result:
  default_result[[i]] <- output_default
  defol_result[[i]] <- output_defol
}

## save lists:
saveRDS(default_result, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_default_result.rds")
saveRDS(defol_result, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_defol_result.rds")


## Checking out results ###
# load:
default_list <- readRDS("/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_default_result.rds")
default_results <- default_list[[1]][1,]
# defolation results:
defol_list <- readRDS("/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_defol_result.rds")

## Convert the defol_list to a dataframe
# extract first member of list:
all_defol <- as.data.frame(defol_list[[1]]) |>
  # add column for list/analysis number:
  mutate(run = 1, .before = 1) |>
  # add columns for defense efficiency and defoliation intensity:
  mutate(defense_efficiency = defense_efficiencies[1], .after = 1) |>
  mutate(defoliation = defoliation, .after = 2)
# run loop cbinding the rest of the list:
for (i in 2:length(defol_list)){
  list_member <- as.data.frame(defol_list[[i]]) |>
    # add column for list/analysis number:
    mutate(run = i, .before = 1) |>
    # add columns for defense efficiency and defoliation intensity:
    mutate(defense_efficiency = defense_efficiencies[i], .after = 1) |>
    mutate(defoliation = defoliation, .after = 2)
  # bind to dataframe:
  all_defol <- rbind(all_defol, list_member)
  rm(list_member)
}


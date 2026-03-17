### Script for testing defense efficiency and defoliation balance parameters

## load in functions:
wd <- setwd("/projectnb/dietzelab/malmborg/PestDefense/")
source(paste0(wd, "/00_PestED_Defoliation.R"))

## add in allocation and turnover (set values):
defense_alloc_percent = 110   # allocation
defense_turnover_percent = 1  # turnover
params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
params$defenseBreakdown = (defense_turnover_percent/100)*366/300/86400*timestep

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
# allocation loop:
for (i in 1:length(defense_efficiencies)){
  # set parameter value:
  params$defenseEfficiency = defense_efficiencies[i]
  
  # matrices of results:
  output_default <- matrix(NA, nrow = length(quants), ncol = 13)
  output_defol <- matrix(NA, nrow = length(quants), ncol = 13)
  colnames(output_default) <- varnames
  colnames(output_defol) <- varnames
  
  # turnover loop:
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

#test <- readRDS("/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_default_result.rds")

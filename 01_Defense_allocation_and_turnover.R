### Script for testing allocation and turnover balance parameters

## load in functions:
wd <- setwd("/projectnb/dietzelab/malmborg/PestDefense/")
source(paste0(wd, "/00_PestED_Defoliation.R"))

## Making a ranges of allocation and turnover values (based on literature):
# quantiles:
quants <- seq(0, 1, by = 0.05)
# testing values based on quantiles:
alloc_spread <- quantile(c(0:30), quants)/365/86400*timestep   # based on % leaf biomass amounts converted to daily then 30 min values, with assumption from no defense PestED that storage and leaf biomass are close to 1:1
turnover_spread <- (quantile(c(0:15), quants))/100*366/300  # based on values from 100-300 growing deg. days decay (Falk et al 2018)

## Matrices of results:
mean_def_biomass <- matrix(NA, nrow = length(alloc_spread), ncol = length(turnover_spread))
# name columns and rows for alloc and turnover numbers:
rownames(mean_def_biomass) <- as.character(alloc_spread)
colnames(mean_def_biomass) <- as.character(turnover_spread)
# list of outputs:
default_outputs_list <- list()

## Looping over all values:
# allocation loop:
for (i in 1:length(alloc_spread)){
  # set parameter value:
  params$defenseAlloc = alloc_spread[i]
  # turnover loop:
  for (j in 1:length(turnover_spread)){
    params$defenseBreakdown = turnover_spread[j]
    # save param values:
    # run default (no defoliation case):
    default <- iterate.SEM(c(0,0,0,0,1,0), years = 5)
    mean_def_biomass[i,j] <- (mean(default[,"Bdefense"]/(default[,"Bdefense"] + default[,"Bleaf"]), na.rm = TRUE))*100
    # run defoliation case:
    #defol <- iterate.SEM(c(0,0,1,1,0), years = 4)
    
    # saving output
    default_outputs_list[[(i*j)]] <- default
  }
}

write.csv(mean_def_biomass, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/mean_def_biomass_percentages_alloc_turnover.csv")

#   default = iterate.SEM(c(0,0,0,1,0), years = 4)
#   plot.SEM(default)
#   check <- default[,"Bdefense"]/(default[,"Bdefense"] + default[,"Bleaf"])
#   plot(check)
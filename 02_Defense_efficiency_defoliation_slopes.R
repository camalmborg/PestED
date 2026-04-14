### Defense Efficiency and Defoliation sensitivities ###

## Load libraries
library(dplyr)

## Load functions
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Load libraries
library(dplyr)

## Load functions
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Setting initial values and values for allocation and turnover
# years:
years = 5
# annual defoliation events:
defol_days <- c(7000)

# set parameter values:
defense_alloc_percent = 90   # allocation percent for whole year
defense_turnover_percent = 1.05  # turnover percent per day
params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
X[8] = X[1]*(0.175/365/86400*timestep)

# defense efficiencies:
def_eff <- c(0.1, 0.25, 0.5, 0.75, 1, 1.5)

# make a list for storing time series:
def_eff_sens <- list()

# get the minimum values of leaf biomass, storage, and wood:
min_bms <- as.matrix(data.frame(def_eff = def_eff,
                                min_leaf = NA,
                                min_store = NA,
                                min_wood = NA))

# loop:
for (i in 1:length(def_eff)){
  # set param values:
  params$defenseEfficiency = de_df$def_eff[i]
  
  # run the model:
  defol_model_run <- iterate.SEM(c(0,0,1,1,0), t.start = defol_days, years = years)
  # extract minimum leaf biomass values:
  df <- as.data.frame(defol_model_run)
  min_bms[i,2] <- min(df$Bleaf)
  min_bms[i,3] <- min(df$Bstore)
  min_bms[i,4] <- min(df$Bwood)
  
  # save it:
  name <- paste0(i, "_def_eff_", def_eff[i])
  def_eff_sens[[name]] <- defol_model_run
  rm(defol_model_run, df)
}
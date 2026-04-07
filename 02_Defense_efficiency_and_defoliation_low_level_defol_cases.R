### Script for testing defense efficiency and defoliation balance parameters ###

# (1) Roll of defense allocation for low-lying annual defoliation 
#  a) 5% defoliation with 0.25-1 defense efficiency
#  b) 10% defoliation with 0.25-1 defense efficiency

## libraries:
library(dplyr)

## load in functions:
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Prepare allocation, turnover parameters and initial values
# years:
years = 5
# annual defoliation events:
defol_days <- c(7000, 24520, 42040, 59560, 77080)
# defense efficiencies:
def_eff <- c(0, 0.1, 0.25, 0.5, 1)

## Set up loop for running low-lying defoliations:
# make lists to collect results:
def_eff_defol_5pc_result <- list()
def_eff_defol_10pc_result <- list()

# loops for running models:
for (i in 1:length(def_eff)){
  # set parameter values:
  defense_alloc_percent = 90   # allocation percent for whole year
  defense_turnover_percent = 1.05  # turnover percent per day
  params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
  params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
  X[8] = X[1]*(0.175/365/86400*timestep)
    
  # set defense efficiency parameter:
  params$defenseEfficiency = def_eff[i]
    
  # run the model for 5% defoliation:
  defol_5pc <- iterate.SEM(c(0,0,0.05,1,0), t.start = defol_days, years = years)
  # run the model for 10% defoliation:
  defol_10pc <- iterate.SEM(c(0,0,0.1,1,0), t.start = defol_days, years = years)
    
  # set list member name:
  name <- paste0(i, "_def_eff_", def_eff[i])
    
  # add results to lists:
  def_eff_defol_5pc_result[[name]] <- defol_5pc
  def_eff_defol_10pc_result[[name]] <- defol_10pc
}


## Making time series plots
# load plotting functions:
source("/projectnb/dietzelab/malmborg/PestDefense/04_Defense_time_series_plots_script.R")

## Processing data for making plots
# selecting desired columns for figures:
variables <- c(2, 1, 4, 8, 7)
cols <- varnames[variables]
names <- c("Wood", "Leaf", "Storage", "Defense", "Density")
plot_units <- units[variables]

# making plot data from alloc_turnover results list:
time_series_data <- SEM_plot_data_fx(def_eff_defol_5pc_result) |>
  # add defense efficiency values for labeling:
  mutate(def_eff = def_eff[model_run])

# model runs chosen for example plots:
lines = c(1:5)
# labels for lines:
labels <- as.character(def_eff)

# making plots:
wood <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 1, runs = lines, labels = labels)
leaf <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 2, runs = lines, labels = labels)
store <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 3, runs = lines, labels = labels)
defense <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 4, runs = lines, labels = labels)
density <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 5, runs = lines, labels = labels)

# combining plots:
combined <- (wood + leaf + store + density) + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "right")
combined


### Defense Allocation and Turnover analyses for time series comparisons ###

# 1-time 100% defoliation with varying allocation/turnover orthogonal and on-axis of param surface

## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

## Load functions
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Set up runs
# allocation and turnover values for time series based on orthogonal/on axis values chosen from heatmap:
alloc_runs <- c(40, 70, 90, 110, 120)
turn_runs <- turnover_spread[c(3, 9, 8, 7, 11)]*100  # make percentage
# make data frame for each run:
alloc_turn_runs <- data.frame(model_run = 1:length(alloc_runs),
                              alloc = alloc_runs,
                              turn = turn_runs,
                              ax_or = c("ax", "or", "ctr", "or", "ax"))


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


## Making time series plots:
# load plotting functions:
source("/projectnb/dietzelab/malmborg/PestDefense/04_Defense_time_series_plots_script.R")

## Processing data for making plots
# selecting desired columns for figures:
variables <- c(2, 1, 4, 8, 7)
cols <- varnames[variables]
names <- c("Wood", "Leaf", "Storage", "Defense", "Density")
plot_units <- units[variables]

# making plot data from alloc_turnover results list:
time_series_data <- SEM_plot_data_fx(alloc_turn_results) |>
  # add allocation and turnover values for trying to deal with labeling:
  mutate(alloc = alloc_runs[model_run]) |>
  mutate(turnover = turn_runs[model_run])

# model runs chosen for example plots:
lines = c(1:5)
# labels for lines:
#a <- unique(time_series_data$alloc)[lines]
a <- alloc_turn_runs$alloc/100/365/86400*timestep
a <- round((a/timestep*86400*100), 2)[lines]  # expressed as percentage of daily storage, matching param surface
t <- unique(time_series_data$turnover)[lines]
labels <- paste0("allocation = ", a, "%, turnover = ", t, "%")

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
#combined

# save plots:
save_dir <- "/projectnb/dietzelab/malmborg/Ch3_PestDefense/allocation_turnover/Figures/"
png(filename = paste0(save_dir, Sys.Date(), "_alloc_turn_biomass_time_series_plots.png"),
    height = 10, width = 15, units = "in", res = 600)
combined
dev.off()


### Archive ###
## Setting up model runs for parallel jobs
# call the run:
#task_id <- as.numeric(Sys.getenv("SGE_TASK_ID"))


#cols <- c("Bwood", "Bleaf", "Bstore", "Bdefense", "density")


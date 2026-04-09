### Script for running multiple simulations of defense SEM with different hierarchy levels ###

## libraries:
library(dplyr)

## load in functions:
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Prepare initialization values
# years:
years = 5   # used only 3 for outbreak case and 5 for ongoing defol case
# annual defoliation events:
#defol_days <- c(7000, 24520)   # for outbreak scenario
defol_days <- c(7000, 24520, 42040, 59560, 77080)   # for low-level defoliation scenario
# defol intensity:
defol_intensities <- c(0.05, 0.1, 0.15, 0.5, 0.75, 1)
defol_int <- defol_intensities[2]

# defense efficiency:
def_effs <- c(0.65, 0.85)
def_eff <- def_effs[2]
defense_alloc_percents <- c(10, 50, 90, 130, 170)
defense_alloc_percent <- defense_alloc_percents[5]

# set parameter values:
defense_turnover_percent = 1.05  # turnover percent per day
params$defenseAlloc = (defense_alloc_percent/100)/365/86400*timestep
params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
X[8] = X[1]*(0.175/365/86400*timestep)
# set defense efficiency parameter:
params$defenseEfficiency = def_eff


# defense hierarchy levels:
dhier <- c(1, 2, 3)

# make list to collect results:
def_hier_result <- list()

# loops for running models:
for (i in 1:length(dhier)){
  #print(paste0(dh, "_before"))
  # set defense param for SEM:
  dh = dhier[i]
  #print(paste0(dh, "_after"))
  
  # run model:
  defol <- iterate.SEM(c(0,0,defol_int,1,0), t.start = defol_days, years = years)
  
  # set list member name:
  name <- paste0("dhier_", i)
  
  # add results to lists:
  def_hier_result[[name]] <- defol
  
  # remove loop results:
  rm(defol)
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

# (1a: 5% defoliation case)
# making plot data from alloc_turnover results list:
time_series_data <- SEM_plot_data_fx(def_hier_result) 

# model runs chosen for example plots:
lines = c(1:length(dhier))
# labels for lines:
labels <- as.character(dhier)

# making plots:
wood <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 1, runs = lines, labels = labels)
leaf <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 2, runs = lines, labels = labels)
store <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 3, runs = lines, labels = labels)
defense <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 4, runs = lines, labels = labels)
density <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 5, runs = lines, labels = labels)

# save groups of plots:
name <- paste0("alloc_", defense_alloc_percent, "_def_eff_", def_eff,"_defol_",defol_int*100, "_hier_run")

# fill with runs:
wood_plots[[name]] <- wood
leaf_plots[[name]] <- leaf
store_plots[[name]] <- store
defense_plots[[name]] <- defense
density_plots[[name]] <- density


## making plots combining wood, leaf, density:
# make a function for removing x-axis for combining plots:
format_plot <- function(plot, x, title) {
  if (x == 0 & title == 0){
    plot + theme(axis.title.x = element_blank(),
                 axis.text.x  = element_blank(),
                 plot.title = element_blank())
  } else if (x == 1 & title == 0){
    plot + theme(plot.title = element_blank())
  } else if (x == 0 & title == 1){
    plot + theme(axis.title.x = element_blank(),
                 axis.text.x  = element_blank())
  }
}

# row labels:
row_label_1 <- wrap_elements(panel = textGrob('Low Allocation', rot=90, gp = gpar(fontsize = 14)))
row_label_2 <- wrap_elements(panel = textGrob('Medium Allocation', rot=90, gp = gpar(fontsize = 14)))
row_label_3 <- wrap_elements(panel = textGrob('High Allocation', rot=90, gp = gpar(fontsize = 14)))
# combining plots:
combined <- (row_label_1 + format_plot(wood_plots[[6]],0,1) + format_plot(leaf_plots[[6]],0,1) + format_plot(density_plots[[6]],0,1) +
               row_label_2 + format_plot(wood_plots[[7]],0,0) + format_plot(leaf_plots[[7]],0,0) + format_plot(density_plots[[7]],0,0) +
               row_label_3 + format_plot(wood_plots[[8]],1,0) + format_plot(leaf_plots[[8]],1,0) + format_plot(density_plots[[8]],1,0)) +
  plot_layout(ncol = 4, widths = c(0.5, 3, 3, 3), guides = "collect") & 
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 
combined


# save plots:
save_dir <- c("/projectnb/dietzelab/malmborg/Ch3_PestDefense/hierarchy/")
png(filename = paste0(save_dir, Sys.Date(),"_def_eff_", def_eff,"_defol_",defol_int*100, "_hier_runs.png"),
    height = 12, width = 16, units = "in", res = 600)
combined
dev.off()


# # IF NOT IN ENV plot lists:
# wood_plots <- list()
# leaf_plots <- list()
# store_plots <- list()
# defense_plots <- list()
# density_plots <- list()

all_plots_defol_100 <- c(wood_plots, leaf_plots, store_plots, defense_plots, density_plots)


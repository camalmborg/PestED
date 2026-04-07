### Script for testing defense efficiency and defoliation balance parameters ###

# (1) Roll of defense allocation for low-lying annual defoliation 
#  a) 5% defoliation with 0-1 defense efficiency
#  b) 10% defoliation with 0-1 defense efficiency
#  c) 15% defoliation with 0-1 defense efficiency

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
def_eff_defol_15pc_result <- list()

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
  # run the model for 15% defoliation:
  defol_15pc <- iterate.SEM(c(0,0,0.15,1,0), t.start = defol_days, years = years)
    
  # set list member name:
  name <- paste0(i, "_def_eff_", def_eff[i])
    
  # add results to lists:
  def_eff_defol_5pc_result[[name]] <- defol_5pc
  def_eff_defol_10pc_result[[name]] <- defol_10pc
  def_eff_defol_15pc_result[[name]] <- defol_15pc
  
  # remove loop results:
  rm(defol_5pc, defol_10pc, defol_15pc)
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
#combined

# save plots:
save_dir <- "/projectnb/dietzelab/malmborg/Ch3_PestDefense/defense_efficiency_defoliation/Figures/"
png(filename = paste0(save_dir, Sys.Date(), "_5pc_annual_defol_time_series_plots.png"),
    height = 10, width = 15, units = "in", res = 600)
combined
dev.off()

# save wood, leaf, density in separate lists:
wood_plots <- list()
leaf_plots <- list()
density_plots <- list()
# save 5pc group:
wood_plots[[1]] <- wood + guides(color = guide_legend(title="Defense Efficiency"))
leaf_plots[[1]] <- leaf + guides(color = "none")
density_plots[[1]] <- density + guides(color = "none")


# (1b: 10% defoliation case)
# making plot data from alloc_turnover results list:
time_series_data <- SEM_plot_data_fx(def_eff_defol_10pc_result) |>
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
#combined

# save plots:
png(filename = paste0(save_dir, Sys.Date(), "_10pc_annual_defol_time_series_plots.png"),
    height = 10, width = 15, units = "in", res = 600)
combined
dev.off()

# save 10pc group:
wood_plots[[2]] <- wood + guides(color = "none") + theme(plot.title = element_blank())
leaf_plots[[2]] <- leaf + guides(color = "none") + theme(plot.title = element_blank())
density_plots[[2]] <- density + guides(color = "none") + theme(plot.title = element_blank())


# (1c: 15% defoliation case)
# making plot data from alloc_turnover results list:
time_series_data <- SEM_plot_data_fx(def_eff_defol_15pc_result) |>
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
#combined

# save plots:
png(filename = paste0(save_dir, Sys.Date(), "_15pc_annual_defol_time_series_plots.png"),
    height = 10, width = 15, units = "in", res = 600)
combined
dev.off()

# save 15pc group:
wood_plots[[3]] <- wood + guides(color = "none") + theme(plot.title = element_blank())
leaf_plots[[3]] <- leaf + guides(color = "none") + theme(plot.title = element_blank())
density_plots[[3]] <- density + guides(color = "none") + theme(plot.title = element_blank())


## making plots combining wood, leaf, density:
# row labels:
row_label_1 <- wrap_elements(panel = textGrob('5% Defoliation', rot=90, gp = gpar(fontsize = 14)))
row_label_2 <- wrap_elements(panel = textGrob('10% Defoliation', rot=90, gp = gpar(fontsize = 14)))
row_label_3 <- wrap_elements(panel = textGrob('15% Defoliation', rot=90, gp = gpar(fontsize = 14)))
# combining plots:
combined <- (row_label_1 + wood_plots[[1]] + leaf_plots[[1]] + density_plots[[1]] +
             row_label_2 + wood_plots[[2]] + leaf_plots[[2]] + density_plots[[2]] +
             row_label_3 + wood_plots[[3]] + leaf_plots[[3]] + density_plots[[3]]) + 
  plot_layout(ncol = 4, widths = c(0.5, 3, 3, 3), guides = "collect") & 
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 
#combined

# save plots:
png(filename = paste0(save_dir, Sys.Date(), "_all_annual_defol_time_series_plots.png"),
    height = 12, width = 16, units = "in", res = 600)
combined
dev.off()





### ARCHIVE ###
# ## combining 0 defense efficiency cases:
# def_eff_0 <- list(d_5 <- as.data.frame(def_eff_defol_5pc_result$`1_def_eff_0`),
#                   d_10 <- as.data.frame(def_eff_defol_10pc_result$`1_def_eff_0`),
#                   d_15 <- as.data.frame(def_eff_defol_15pc_result$`1_def_eff_0`))
# 
# # model runs chosen for example plots:
# lines = c(1:3)
# # labels for lines:
# labels <- as.character(c(5,10,15))
# 
# # making plot data from alloc_turnover results list:
# time_series_data <- SEM_plot_data_fx(def_eff_0)
# 
# density <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 5, runs = lines, labels = labels)
# 

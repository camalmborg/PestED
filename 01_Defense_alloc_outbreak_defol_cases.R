### Script for testing allocation and outbreak defoliation ###

# (1) Role of defense allocation for 2-year outbreak defoliation 
#  a) 50% defoliation 
#  b) 75% defoliation 
#  c) 100% defoliation

# consistent turnover, changing allocation
# testing at two defense efficiencies (not at saturation)

## libraries:
library(dplyr)

## load in functions:
source("/projectnb/dietzelab/malmborg/PestDefense/00_PestED_Defoliation.R")

## Prepare initialization values
# years:
years = 5
# annual defoliation events:
defol_days <- c(7000, 24520)
# defense efficiency:
def_effs <- c(0.65, 0.85)
def_eff <- def_effs[2]

# allocation percent values to test:
#defense_alloc_percent <- c(60, 70, 80, 90, 100, 110, 120)  # first runs
defense_alloc_percent <- c(10, 50, 90, 130, 170)

## Set up loop for running outbreak defoliations:
# make lists to collect results:
alloc_defol_50pc_result <- list()
alloc_defol_75pc_result <- list()
alloc_defol_100pc_result <- list()

# loops for running models:
for (i in 1:length(defense_alloc_percent)){
  # set parameter values:
  alloc_percent = defense_alloc_percent[i]  # allocation percent for whole year
  defense_turnover_percent = 1.05  # turnover percent per day
  params$defenseAlloc = (alloc_percent/100)/365/86400*timestep
  params$defenseBreakdown = (defense_turnover_percent/100)/86400*timestep
  X[8] = X[1]*(0.175/365/86400*timestep)
  
  # set defense efficiency parameter:
  params$defenseEfficiency = def_eff
  
  # run the model for 50% defoliation:
  defol_50pc <- iterate.SEM(c(0,0,0.5,1,0), t.start = defol_days, years = years)
  # run the model for 75% defoliation:
  defol_75pc <- iterate.SEM(c(0,0,0.75,1,0), t.start = defol_days, years = years)
  # run the model for 100% defoliation:
  defol_100pc <- iterate.SEM(c(0,0,1,1,0), t.start = defol_days, years = years)
  
  # set list member name:
  name <- paste0(i, "_alloc_", defense_alloc_percent[i])
  
  # add results to lists:
  alloc_defol_50pc_result[[name]] <- defol_50pc
  alloc_defol_75pc_result[[name]] <- defol_75pc
  alloc_defol_100pc_result[[name]] <- defol_100pc
  
  # remove loop results:
  rm(defol_50pc, defol_75pc, defol_100pc)
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
time_series_data <- SEM_plot_data_fx(alloc_defol_50pc_result) |>
  # add defense alloc values for labeling:
  mutate(alloc = defense_alloc_percent[model_run])

# model runs chosen for example plots:
lines = c(1:length(defense_alloc_percent))
# labels for lines:
#labels <- as.character(c(0.16, 0.19, 0.22, 0.25, 0.27, 0.30, 0.33))
labels <- as.character(c(0.03, 0.14, 0.25, 0.36, 0.47))

# making plots:
wood <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 1, runs = lines, labels = labels)
leaf <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 2, runs = lines, labels = labels)
store <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 3, runs = lines, labels = labels)
defense <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 4, runs = lines, labels = labels)
density <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 5, runs = lines, labels = labels)

# combining plots:
combined <- (wood + leaf + store + density) + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "right") & guides(color = guide_legend(title="Defense Allocation\n(% Daily)"))
#combined

# save plots:
save_dir <- "/projectnb/dietzelab/malmborg/Ch3_PestDefense/allocation_turnover/Figures/"
png(filename = paste0(save_dir, Sys.Date(), "_50pc_outbreak_defol_def_eff_", def_eff,"_time_series_plots.png"),
    height = 10, width = 15, units = "in", res = 600)
combined
dev.off()

# save wood, leaf, density in separate lists:
wood_plots <- list()
leaf_plots <- list()
density_plots <- list()
# save 5pc group:
wood_plots[[1]] <- wood + guides(color = guide_legend(title=" Defense Allocation\n(% Daily)"))
leaf_plots[[1]] <- leaf + guides(color = "none")
density_plots[[1]] <- density + guides(color = "none")


# (1b: 10% defoliation case)
# making plot data from alloc_turnover results list:
time_series_data <- SEM_plot_data_fx(alloc_defol_75pc_result) |>
  # add defense alloc values for labeling:
  mutate(alloc = defense_alloc_percent[model_run])

# making plots:
wood <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 1, runs = lines, labels = labels)
leaf <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 2, runs = lines, labels = labels)
store <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 3, runs = lines, labels = labels)
defense <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 4, runs = lines, labels = labels)
density <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 5, runs = lines, labels = labels)

# combining plots:
combined <- (wood + leaf + store + density) + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "right") & guides(color = guide_legend(title="Defense Allocation\n(% Daily)"))
#combined

# save plots:
png(filename = paste0(save_dir, Sys.Date(), "_75pc_outbreak_defol_def_eff_", def_eff,"_time_series_plots.png"),
    height = 10, width = 15, units = "in", res = 600)
combined
dev.off()

# save 10pc group:
wood_plots[[2]] <- wood + guides(color = "none") + theme(plot.title = element_blank())
leaf_plots[[2]] <- leaf + guides(color = "none") + theme(plot.title = element_blank())
density_plots[[2]] <- density + guides(color = "none") + theme(plot.title = element_blank())


# (1c: 15% defoliation case)
# making plot data from alloc_turnover results list:
time_series_data <- SEM_plot_data_fx(alloc_defol_100pc_result) |>
  # add defense alloc values for labeling:
  mutate(alloc = defense_alloc_percent[model_run])

# making plots:
wood <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 1, runs = lines, labels = labels)
leaf <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 2, runs = lines, labels = labels)
store <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 3, runs = lines, labels = labels)
defense <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 4, runs = lines, labels = labels)
density <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 5, runs = lines, labels = labels)

# combining plots:
combined <- (wood + leaf + store + density) + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "right") & guides(color = guide_legend(title="Defense Allocation\n(% Daily)"))
#combined

# save plots:
png(filename = paste0(save_dir, Sys.Date(), "_100pc_outbreak_defol_def_eff_", def_eff,"_time_series_plots.png"),
    height = 10, width = 15, units = "in", res = 600)
combined
dev.off()

# save 15pc group:
wood_plots[[3]] <- wood + guides(color = "none") + theme(plot.title = element_blank())
leaf_plots[[3]] <- leaf + guides(color = "none") + theme(plot.title = element_blank())
density_plots[[3]] <- density + guides(color = "none") + theme(plot.title = element_blank())


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
row_label_1 <- wrap_elements(panel = textGrob('50% Defoliation', rot=90, gp = gpar(fontsize = 14)))
row_label_2 <- wrap_elements(panel = textGrob('75% Defoliation', rot=90, gp = gpar(fontsize = 14)))
row_label_3 <- wrap_elements(panel = textGrob('100% Defoliation', rot=90, gp = gpar(fontsize = 14)))
# combining plots:
combined <- (row_label_1 + format_plot(wood_plots[[1]],0,1) + format_plot(leaf_plots[[1]],0,1) + format_plot(density_plots[[1]],0,1) +
               row_label_2 + format_plot(wood_plots[[2]],0,0) + format_plot(leaf_plots[[2]],0,0) + format_plot(density_plots[[2]],0,0) +
               row_label_3 + format_plot(wood_plots[[3]],1,0) + format_plot(leaf_plots[[3]],1,0) + format_plot(density_plots[[3]],1,0)) + 
  plot_layout(ncol = 4, widths = c(0.5, 3, 3, 3), guides = "collect") & 
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 
#combined

# save plots:
png(filename = paste0(save_dir, Sys.Date(), "_all_outbreak_defol_def_eff_", def_eff,"_time_series_plots.png"),
    height = 12, width = 16, units = "in", res = 600)
combined
dev.off()



## Bar plots
# process and combine outputs:
bar_plot_data <- rbind(SEM_bar_data_fx(alloc_defol_50pc_result, group = "50%"),
                       SEM_bar_data_fx(alloc_defol_75pc_result, group = "75%"),
                       SEM_bar_data_fx(alloc_defol_100pc_result, group = "100%"))

# labels specifying groups:
labels <- as.character(c(0.03, 0.14, 0.25, 0.36, 0.47))
# number of runs:
lines = c(1:length(defense_alloc_percent))

# make plots:
wood <- SEM_bar_plot_fx(bar_plot_data, cols = cols, var = 1, runs = lines, labels = labels) +
  labs(x = "Allocation")
leaf <- SEM_bar_plot_fx(bar_plot_data, cols = cols, var = 2, runs = lines, labels = labels) +
  labs(x = "Allocation")
store <- SEM_bar_plot_fx(bar_plot_data, cols = cols, var = 3, runs = lines, labels = labels) +
  labs(x = "Allocation")
defense <- SEM_bar_plot_fx(bar_plot_data, cols = cols, var = 4, runs = lines, labels = labels) +
  labs(x = "Allocation")
density <- SEM_bar_plot_fx(bar_plot_data, cols = cols, var = 5, runs = lines, labels = labels) +
  labs(x = "Allocation") 


# combining bar plots:
combined <- (leaf + store + density) + 
  plot_layout(ncol = 3, guides = "collect") & 
  theme(plot.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 12)) & guides(fill = guide_legend(title = "2-year Outbreak\nAnnual Defoliation"))
#combined
combined + plot_annotation(title = "Minimum Biomass and Tree Densities Experienced During Defoliator Outbreak Scenarios")

# save bar plot:
png(filename = paste0(save_dir, Sys.Date(), "_all_outbreak_defol_", def_eff,"_bar_plots.png"),
    height = 4, width = 12, units = "in", res = 600)
combined + plot_annotation(title = "Minimum Biomass and Tree Densities Experienced During Defoliator Outbreak Scenarios")
dev.off()

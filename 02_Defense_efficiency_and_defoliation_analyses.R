### Defense Efficiency and Defoliation time series ###

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
def_eff <- c(0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3)
# defoliation amounts:
defol <- c(0.25, 0.5, 0.75, 1)

# make data frame for looping:
de_df <- data.frame(model_run = 1:(length(defol)*length(def_eff)),
                    defol = rep(defol, each = length(def_eff)),
                    def_eff = rep(def_eff, length(defol)))

# make a list for storing time series:
def_eff_defol_results <- list()

# loop:
for (i in 1:nrow(de_df)){
  # set param values:
  params$defenseEfficiency = de_df$def_eff[i]
  
  # run the model:
  defol_model_run <- iterate.SEM(c(0,0,de_df$defol[i],1,0), t.start = defol_days, years = years)
  
  # save it:
  name <- paste0(i, "_defol_", de_df$defol[i], 
                 "_def_eff_", de_df$def_eff[i])
  def_eff_defol_results[[name]] <- defol_model_run
  rm(defol_model_run)
}

# separate by defense efficiency:
eff_10 <- def_eff_defol_results[grep("eff_0.1", names(def_eff_defol_results))]
eff_25 <- def_eff_defol_results[grep("eff_0.25", names(def_eff_defol_results))]
eff_50 <- def_eff_defol_results[grep("eff_0.5", names(def_eff_defol_results))]
eff_75 <- def_eff_defol_results[grep("eff_0.75", names(def_eff_defol_results))]
eff_100 <- def_eff_defol_results[grep("eff_1$", names(def_eff_defol_results))]
eff_150 <- def_eff_defol_results[grep("eff_1.5", names(def_eff_defol_results))]
#eff_200 <- def_eff_defol_results[grep("eff_2", names(def_eff_defol_results))]
#eff_300 <- def_eff_defol_results[grep("eff_3", names(def_eff_defol_results))]

# make big list of lists to make plots:
big_list <- list(eff_10, eff_25, eff_50, eff_75, eff_100, eff_150)


## Making time series plots:
# load plotting functions:
source("/projectnb/dietzelab/malmborg/PestDefense/04_Defense_time_series_plots_script.R")

## Processing data for making plots
# selecting desired columns for figures:
variables <- c(2, 1, 4, 8, 7)
cols <- varnames[variables]
names <- c("Wood", "Leaf", "Storage", "Defense", "Density")
plot_units <- units[variables]

# plot lists:
wood_plots <- list()
leaf_plots <- list()
store_plots <- list()
density_plots <- list()

# loop for making plots:
for (i in 1:length(big_list)){
  # open list:
  time_series_data <- SEM_plot_data_fx(big_list[[i]]) |>
    # add defoliation values for labeling:
    mutate(defol_intensity = defol[model_run])
  
  # model runs chosen for example plots:
  lines = c(1:length(unique(time_series_data$model_run)))
  # labels for lines:
  labels <- paste0(as.character(unique(time_series_data$defol_intensity)[lines]*100), "%")
  
  # making plots:
  wood <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 1, runs = lines, labels = labels) + 
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 12))
  leaf <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 2, runs = lines, labels = labels) +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 12))
  store <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 3, runs = lines, labels = labels) +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 12))
  density <- time_series_plot_fx(ts = time_series_data, cols = cols, var = 5, runs = lines, labels = labels) +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 12))
  
  # fill plot lists:
  name <- as.character(def_eff[i])
  wood_plots[[name]] <- wood 
  leaf_plots[[name]] <- leaf
  store_plots[[name]] <- store
  density_plots[[name]] <- density
  
  rm(wood, leaf, store, density)
}

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


# combining plots:
# row labels:
row_label_1 <- wrap_elements(panel = textGrob('Efficiency = 0.1', rot=90, gp = gpar(fontsize = 12)))
row_label_2 <- wrap_elements(panel = textGrob('Efficiency = 0.25', rot=90, gp = gpar(fontsize = 12)))
row_label_3 <- wrap_elements(panel = textGrob('Efficiency = 0.5', rot=90, gp = gpar(fontsize = 12)))
row_label_4 <- wrap_elements(panel = textGrob('Efficiency = 0.75', rot=90, gp = gpar(fontsize = 12)))
row_label_5 <- wrap_elements(panel = textGrob('Efficiency = 1', rot=90, gp = gpar(fontsize = 12)))
row_label_6 <- wrap_elements(panel = textGrob('Efficiency = 1.5', rot=90, gp = gpar(fontsize = 12)))

# combining plots:
combined <- (row_label_1 + format_plot(wood_plots[[1]], 0, 1) + format_plot(leaf_plots[[1]], 0, 1) + format_plot(store_plots[[1]], 0, 1) + format_plot(density_plots[[1]], 0, 1) +     
             row_label_2 + format_plot(wood_plots[[2]], 0, 0) + format_plot(leaf_plots[[2]], 0, 0) + format_plot(store_plots[[2]], 0, 0) + format_plot(density_plots[[2]], 0, 0) +
             row_label_3 + format_plot(wood_plots[[3]], 0, 0) + format_plot(leaf_plots[[3]], 0, 0) + format_plot(store_plots[[3]], 0, 0) + format_plot(density_plots[[3]], 0, 0) +
             #row_label_4 + format_plot(wood_plots[[4]], 0, 0) + format_plot(leaf_plots[[4]], 0, 0) + format_plot(store_plots[[4]], 0, 0) + format_plot(density_plots[[4]], 0, 0) +
             row_label_5 + format_plot(wood_plots[[5]], 0, 0) + format_plot(leaf_plots[[5]], 0, 0) + format_plot(store_plots[[5]], 0, 0) + format_plot(density_plots[[5]], 0, 0) +
             row_label_6 + format_plot(wood_plots[[6]], 1, 0) + format_plot(leaf_plots[[6]], 1, 0) + format_plot(store_plots[[6]], 1, 0) + format_plot(density_plots[[6]], 1, 0)) +
  plot_layout(ncol = 5, widths = c(0.5, 3, 3, 3, 3), guides = "collect") & 
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) & guides(color = guide_legend(title="Defoliation Intensity"))
#combined

# save plots:
save_dir <- "/projectnb/dietzelab/malmborg/Ch3_PestDefense/defense_efficiency_defoliation/Figures/"
png(filename = paste0(save_dir, Sys.Date(), "_all_defol_25-100_time_series_plots.png"),
    height = 12, width = 16, units = "in", res = 600)
combined
dev.off()



### ARCHIVE ###
# default_result <- list()
# defol_result <- list()
# 
# ## Looping over all values:
# # defense efficiency loop:
# for (i in 1:length(defense_efficiencies)){
#   # set parameter value:
#   params$defenseEfficiency = defense_efficiencies[i]
#   
#   # matrices of results:
#   output_default <- matrix(NA, nrow = length(quants), ncol = 13)
#   output_defol <- matrix(NA, nrow = length(quants), ncol = 13)
#   colnames(output_default) <- varnames
#   colnames(output_defol) <- varnames
#   
#   # defoliation magnitude loop:
#   for (j in 1:length(defoliation)){
#     # run default (no defoliation case):
#     default <- iterate.SEM(c(0,0,0,1,0), years = 4)
#     defol <- iterate.SEM(c(0,0,defoliation[j],1,0), years = 4)
#     
#     # get the output summaries:
#     output_default[j,] <- 1-apply(default,2,min)/apply(default,2,max)
#     output_defol[j,] <- 1-apply(defol,2,min)/apply(defol,2,max)
#   }
#   # all result:
#   default_result[[i]] <- output_default
#   defol_result[[i]] <- output_defol
# }
# 
# ## save lists:
# saveRDS(default_result, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_default_result.rds")
# saveRDS(defol_result, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_defol_result.rds")
# 
# 
# ## Checking out results ###
# # load:
# default_list <- readRDS("/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_default_result.rds")
# default_results <- default_list[[1]][1,]
# # defolation results:
# defol_list <- readRDS("/projectnb/dietzelab/malmborg/Ch3_PestDefense/DED_defol_result.rds")
# 
# ## Convert the defol_list to a dataframe
# # extract first member of list:
# all_defol <- as.data.frame(defol_list[[1]]) |>
#   # add column for list/analysis number:
#   mutate(run = 1, .before = 1) |>
#   # add columns for defense efficiency and defoliation intensity:
#   mutate(defense_efficiency = defense_efficiencies[1], .after = 1) |>
#   mutate(defoliation = defoliation, .after = 2)
# # run loop cbinding the rest of the list:
# for (i in 2:length(defol_list)){
#   list_member <- as.data.frame(defol_list[[i]]) |>
#     # add column for list/analysis number:
#     mutate(run = i, .before = 1) |>
#     # add columns for defense efficiency and defoliation intensity:
#     mutate(defense_efficiency = defense_efficiencies[i], .after = 1) |>
#     mutate(defoliation = defoliation, .after = 2)
#   # bind to dataframe:
#   all_defol <- rbind(all_defol, list_member)
#   rm(list_member)
# }
# 
# 



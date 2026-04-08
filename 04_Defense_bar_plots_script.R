### Making bar plots for looking at finer details of results ###

## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

## Load other plot functions:
source("/projectnb/dietzelab/malmborg/PestDefense/04_Defense_time_series_plots_script.R")


## Making bar data:
#'@param output_list = list from model runs loop
#'@param group = putting in group identifier
SEM_bar_data_fx <- function(output_list, group){
  # make output:
  output <- SEM_plot_data_fx(output_list)
  
  # group data to get minimum values of each variable:
  bar_data <- output |>
    # group by model:
    group_by(model_run) |>
    # get minimum in group:
    summarise(across(all_of(cols), ~ min(.x, na.rm = TRUE))) |>
    # add group identifier
    mutate(group = as.character(group))
    
  return(bar_data)
}

## bar plot maker function:
#'@param bar_data = bar plot data from SEM_bar_data_fx
#'@param cols = character vector, which columns from time series data you want, e.g. "Bleaf"
#'@param var = character vector, which variable that is, e.g "Leaf Biomass"
#'@param runs = numeric vector, which model runs you would like to include
#'@param labels = character vector, legend labels
SEM_bar_plot_fx <- function(bar_data, cols, var, runs, labels){
  # set up data:
  bar_plot_data <- bar_data
  # set up plot data:
  plot_data <- bar_plot_data |>
    # select desired variable:
    select(-c(cols)[-c(var)]) |>
    # rename column for making plot:
    rename(value = cols[var]) |>
    # remake group ordered:
    mutate(group = factor(group, levels = group_order)) |>
    # remove na rows for plotting (if applicable)
    drop_na(value)
  
  # color palette:
  # generate colors based on the number of lines:
  n_bars <- length(unique(plot_data$group))
  bar_colors <- palette.colors(n_bars, "Okabe-Ito")
  
  # names for plots:
  x_axis <- "Model"
  y_axis <- plot_units[var]
  plot_title <- names[var]
  
  # make bar plot:
  bar_plot <- ggplot(plot_data, aes(x = factor(model_run), y = value, fill = group)) +
    geom_col(position = "dodge", width = 0.65) +
    # add colors:
    scale_fill_manual(values = bar_colors) +
    labs(x = x_axis, y = y_axis, title = plot_title,
         fill = "") +
    # label x axis:
    scale_x_discrete(labels = labels) +
    # theme details:
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 14),
          legend.position = "right",
          legend.text = element_text(size = 12),
          panel.grid = element_blank())
    
  return(bar_plot)
}

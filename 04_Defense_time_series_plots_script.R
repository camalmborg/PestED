### Making results plots for comparing outputs ###

## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

## Functions for setting up data for time series
# processing data function:
#'@param output = output time series = dataframe
#'@param cols = which columns you want to keep from each time series = character vector
#'@param model_run = which model run you are on = numeric; used for list number i in loop for SEM_plot_data_fx
#'@param years = number of years = numeric
SEM_output_fx <- function(output, cols, model_run, years){
  model_outputs <- as.data.frame(output) |>
    # select columns for plots:
    select(all_of(cols)) |>
    # add a column for analysis group:
    mutate(model_run = model_run, .before = 1) |>
    # adding column for timestep:
    mutate(timestep = 1:nrow(output), .before = 2) |>
    # add column for years:
    mutate(year = rep(1:years, each = nrow(output)/years), .before = 3)
  return(model_outputs)
}

# function for processing all datasets in group:
#'@param output_list = list object with outputs from SEM model runs
SEM_plot_data_fx <- function(output_list){
  # make new list for processed data:
  processed <- list()
  # loop for going through each with output data processor:
  for(i in 1:length(output_list)){
    out <- SEM_output_fx(output_list[[i]], cols, i, years)
    processed[[i]] <- out
  }
  # rbind list:
  plot_data <- do.call(rbind, processed)
  rm(processed)
  return(plot_data)
}


## Function for making figures
#'@param ts = time series data from SEM_plot_data_fx
#'@param cols = character vector, which columns from time series data you want, e.g. "Bleaf"
#'@param var = character vector, which variable that is, e.g "Leaf Biomass"
#'@param runs = numeric vector, which model runs you would like to include on plot, keeping number of lines 5 or less
#'@param labels = character vector, legend labels
time_series_plot_fx <- function(ts, cols, var, runs, labels){
  time_series_data <- ts
  # set up plot data:
  plot_data <- time_series_data |>
    # select desired variable:
    select(-c(cols)[-c(var)]) |>
    # rename column for making plot:
    rename(value = cols[var]) |>
    # select the models you want:
    filter(model_run == runs) |>
    # remove na rows for plotting (if applicable)
    drop_na(value)
  
  # color palette:
  line_palette <- colorRampPalette(c("blue", "red"))
  # generate colors based on the number of lines
  n_lines <- length(unique(plot_data$model_run))
  line_colors <- line_palette(n_lines)
  
  # names for plots:
  x_axis <- "Year"
  y_axis <- plot_units[var]
  plot_title <- names[var]
  
  # making plots:
  var_plot <- ggplot(data = plot_data, aes(x = timestep, y = value, 
                                            group = model_run, 
                                            color = as.factor(model_run))) +
    geom_line(linewidth = 0.75) +
    scale_color_manual(values = line_colors, labels = labels) +
    labs(title = plot_title,
         x = x_axis, y = y_axis,
         color = "") +
    # make x-axis labeled years:
    scale_x_continuous(breaks = seq(1, max(plot_data$timestep), by = length(plot_data$timestep)/max(plot_data$year)),
                       labels = c(1:max(plot_data$year))) +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 14),
          legend.position = "right",
          legend.text = element_text(size = 12),
          panel.grid = element_blank())
  
  return(var_plot)
}




### Archive ###
# dnd <- SEM_output_fx(default_no_defense, cols = cols, model_run = 1, years = 5)
# dwd <- SEM_output_fx(default_with_defense, cols = cols, model_run = 2, years = 5)
# def_nd <- SEM_output_fx(defol_no_def, cols = cols, model_run = 3, years = 5)
# def <- SEM_output_fx(defol, cols = cols, model_run = 4, years = 5)
# def2 <- SEM_output_fx(defol2, cols = cols, model_run = 5, years = 5)
# def3 <- SEM_output_fx(defol3, cols = cols, model_run = 6, years = 5)
# #all <- rbind(dnd, dwd, def_nd, def, def2, def3)
# all <- rbind(def_nd, def, def2, def3)
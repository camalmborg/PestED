### Making results plots for comparing outputs ###

## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

## Processing data for making plots
# selecting desired columns for figures:
cols <- c("Bwood", "Bleaf", "Bstore", "Bdefense", "density")

# processing data function:
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

# plot data:
time_series_data <- SEM_plot_data_fx(alloc_turn_results)

## Making figures:
time_series_plot_fx <- function(cols, var, runs){
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
  n_lines <- length(unique(bleaf$model_run))
  line_colors <- line_palette(n_lines)
  
  # names for plots:
  x_axis <- "Year"
  y_axis <- ""
  
  # making plots:
  test_plot <- ggplot(data = plot_data, aes(x = timestep, y = value, 
                                            group = model_run, 
                                            color = as.factor(model_run))) +
    geom_line(linewidth = 0.75) +
    scale_color_manual(values = line_colors) +
    theme_bw() +
    theme(legend.position = "right",
          panel.grid = element_blank())
}

# selecting and processing for plots:
var <- grep("Bleaf", cols)

plot_data <- time_series_data |>
  # select desired variable:
  select(-c(cols)[-c(var)]) |>
  # rename column for making plot:
  rename(value = cols[var]) |>
  # remove na rows for plotting (if applicable)
  drop_na(value) |>
  # select the models you want:
  filter(model_run == c(1, 3, 6, 8)) 

# color palette:
line_palette <- colorRampPalette(c("blue", "red"))
# generate colors based on the number of lines
n_lines <- length(unique(bleaf$model_run))
line_colors <- line_palette(n_lines)

## Making plots
test_plot <- ggplot(data = plot_data, aes(x = timestep, y = value, 
                                      group = model_run, 
                                      color = as.factor(model_run))) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = line_colors) +
  scale_x_continuous(breaks = plot_data$timestep, labels = plot_data$year) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank())
test_plot


### Archive ###
# dnd <- SEM_output_fx(default_no_defense, cols = cols, model_run = 1, years = 5)
# dwd <- SEM_output_fx(default_with_defense, cols = cols, model_run = 2, years = 5)
# def_nd <- SEM_output_fx(defol_no_def, cols = cols, model_run = 3, years = 5)
# def <- SEM_output_fx(defol, cols = cols, model_run = 4, years = 5)
# def2 <- SEM_output_fx(defol2, cols = cols, model_run = 5, years = 5)
# def3 <- SEM_output_fx(defol3, cols = cols, model_run = 6, years = 5)
# #all <- rbind(dnd, dwd, def_nd, def, def2, def3)
# all <- rbind(def_nd, def, def2, def3)
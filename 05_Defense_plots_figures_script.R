### Making results plots for comparing outputs ###

## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

## Loading data

## Processing data for making plots
# selecting desired columns for figures:
cols <- c("Bwood", "Bleaf", "Bstore", "Bdefense")

# processing data:
SEM_output_fx <- function(output, cols, model_run, years){
  biomass_estimates <- as.data.frame(output) |>
    # select columns for plots:
    select(all_of(cols)) |>
    # add a column for analysis group:
    mutate(model_run = model_run, .before = 1) |>
    # adding column for timestep:
    mutate(timestep = 1:nrow(output), .before = 2) |>
    # add column for years:
    mutate(year = rep(1:years, each = nrow(output)/years), .before = 3)
}

dnd <- SEM_output_fx(default_no_defense, cols = cols, model_run = 1, years = 5)
dwd <- SEM_output_fx(default_with_defense, cols = cols, model_run = 2, years = 5)
def_nd <- SEM_output_fx(defol_no_def, cols = cols, model_run = 3, years = 5)
def <- SEM_output_fx(defol, cols = cols, model_run = 4, years = 5)
def2 <- SEM_output_fx(defol2, cols = cols, model_run = 5, years = 5)
def3 <- SEM_output_fx(defol3, cols = cols, model_run = 6, years = 5)
#all <- rbind(dnd, dwd, def_nd, def, def2, def3)
all <- rbind(dwd, def_nd, def, def2, def3)

# selecting and processing for plots:
var <- grep("Bleaf", cols)

bleaf <- all |>
  # select desired variable:
  select(-c(cols)[-c(var)]) |>
  # rename column for making plot:
  rename(value = cols[var]) #|>
  #drop_na(value)

## Making plots
test_plot <- ggplot(data = bleaf, aes(x = timestep, y = value, group = model_run, color = model_run)) +
  geom_line(linewidth = 1) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank())
test_plot

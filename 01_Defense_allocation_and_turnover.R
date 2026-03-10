### Script for testing allocation and turnover balance parameters

## load in functions:
wd <- setwd("/projectnb/dietzelab/malmborg/PestDefense/")
source(paste0(wd, "/00_PestED_Defoliation.R"))

## Making a ranges of allocation and turnover values (based on literature):
# quantiles:
quants <- seq(0, 1, by = 0.05)
# testing values based on quantiles:
alloc_spread <- (quantile(c(0:200), quants)/100)/365/86400*timestep  # based on % leaf biomass amounts converted to daily then 30 min values, with assumption from no defense PestED that storage and leaf biomass are close to 1:1
turnover_spread <- (quantile(c(0:10), quants)/100)*366/300  # based on values from 100-300 growing deg. days decay (Falk et al 2018)

## Matrices of results:
mean_def_biomass <- matrix(NA, nrow = length(alloc_spread), ncol = length(turnover_spread))
min_leaf_biomass <- matrix(NA, nrow = length(alloc_spread), ncol = length(turnover_spread))
min_store_biomass <- matrix(NA, nrow = length(alloc_spread), ncol = length(turnover_spread))

# name columns and rows for alloc and turnover numbers:
rownames(mean_def_biomass) <- as.character(alloc_spread)
colnames(mean_def_biomass) <- as.character(turnover_spread)
# list of outputs:
default_outputs_list <- list()
defol_outputs_list <- list()

# n_peaks <- list()
# n_valleys <- list()

## Looping over all values:
# allocation loop:
for (i in 1:length(alloc_spread)){
  # set parameter value:
  params$defenseAlloc = alloc_spread[i]
  # turnover loop:
  for (j in 1:length(turnover_spread)){
    params$defenseBreakdown = turnover_spread[j]/86400*timestep
    # save param values:
    # run default (no defoliation case):
    default <- iterate.SEM(c(0,0,0,0,1,0), years = 4)
    mean_def_biomass[i,j] <- mean(default[,"Bdefense"]/(default[,"Bdefense"] + default[,"Bleaf"]), na.rm = TRUE)
    min_leaf_biomass[i,j] <- min(default[,"Bleaf"], na.rm = T)
    min_store_biomass[i,j] <- min(default[,"Bstore"], na.rm = T)
    # run defoliation case:
    #defol <- iterate.SEM(c(0,0,1,1,0), years = 4)
    # saving output
    #default_outputs_list[[(i*j)]] <- default
  }
}

write.csv(mean_def_biomass, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/mean_def_biomass_alloc_turnover.csv")
write.csv(min_leaf_biomass, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/min_leaf_biomass_alloc_turnover.csv")
write.csv(min_store_biomass, file = "/projectnb/dietzelab/malmborg/Ch3_PestDefense/min_store_biomass_alloc_turnover.csv")



## Check out results and view heat map
# load superheat library:
library(dplyr)
library(superheat)
library(grid)
# load the csv of allocation/turnover tests:
alloc_turn <- read.csv("/projectnb/dietzelab/malmborg/Ch3_PestDefense/mean_def_biomass_alloc_turnover.csv", row.names = 1) |>
  # multiply by 100 to show mean defense chemistry in percentage of leaf biomass:
  mutate(across(where(is.numeric), ~ .x * 100)) |>
  # round for nicer display:
  mutate(across(where(is.numeric), ~round(., digits = 3))) |>
  # remove 0 allocation and defense row/column:
  slice(-1) |>
  select(-1)

# renaming columns and rows:
rownames(alloc_turn) <- as.character(seq(10, 200, by = 10))
colnames(alloc_turn) <- as.character(seq(0.5, 10, by = 0.5))

# values for color scaling in heatmap
vals <- c(1, 3, 7, 20, 28, 40, 50)
vals_scaled <- (vals - min(vals)) / (max(vals) - min(vals))

# highlight groups between 7-28% DW biomass defense values (Lindroth):
# mean and SD to compute highs and lows:
mean_defense <- mean(7:28)
sd_defense <- sd(7:28)
Bdefense_high <- mean_defense + sd_defense
Bdefense_low <- mean_defense - sd_defense
# make highlight group:
highlight <- alloc_turn < Bdefense_high & alloc_turn > Bdefense_low
highlight <- gsub("TRUE", "black", highlight)
highlight <- gsub("FALSE", "grey40", highlight)
highlight <- matrix(highlight, ncol = ncol(alloc_turn))

# make heatmap:
superheat(alloc_turn,
          # add text:
          X.text = as.matrix(round(alloc_turn, 2)),
          X.text.size = 4,
          # color to highlight certain values:
          X.text.col = highlight,
          # add color:
          heat.pal = c("skyblue1", "lightblue1", "lightcyan","white", "pink", "lightpink", "indianred1"),
          heat.pal.values = vals_scaled,
          legend = FALSE,
          # row and column labels:
          # row title
          row.title = "Defense Allocation (% annual storage)",
          row.title.size = 4,
          # col title
          column.title = "Rate of Turnover (% per unit time)",
          column.title.size = 4,
          # change size of labels:
          left.label.size = 0.1,
          bottom.label.size = 0.1,
          # change label text size:
          left.label.text.size = 4,
          bottom.label.text.size = 4)

# load minimum leaf biomass and storage biomass results:
min_Bleaf <- alloc_turn <- read.csv("/projectnb/dietzelab/malmborg/Ch3_PestDefense/min_leaf_biomass_alloc_turnover.csv", row.names = 1) |>
  # round for nicer display:
  mutate(across(where(is.numeric), ~round(., digits = 3))) |>
  # remove 0 allocation and defense row/column:
  slice(-1) |>
  select(-1)
min_Bstore <- read.csv("/projectnb/dietzelab/malmborg/Ch3_PestDefense/min_store_biomass_alloc_turnover.csv", row.names = 1) |>
  # round for nicer display:
  mutate(across(where(is.numeric), ~round(., digits = 3))) |>
  # remove 0 allocation and defense row/column:
  slice(-1) |>
  select(-1)


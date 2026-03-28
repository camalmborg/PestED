### SANDBOX ###

### migrating from ncdf library to ncdf4----
# how to get the same thing out out nc files that you get from print.ncdf(met):
# Dimensions
for (d in names(met$dim)) {
  cat("Dimension:", d, "\n")
  print(met$dim[[d]])
  cat("\n")
}

# Variables
for (v in names(met$var)) {
  cat("Variable:", v, "\n")
  print(met$var[[v]])
  cat("\n")
}

###
default_result <- default_list[[1]][1,]

# turning a list into data frame:
def_eff_defol_output <- data.frame(def_eff = rep(defense_efficiencies[1], length = length(defense_efficiencies)),
                                   defoliation = defoliation) |>
  cbind(defol_list[[1]])
# loop over rest of list:
for (i in 2:length(defol_list)){
  out <- data.frame(def_eff = rep(defense_efficiencies[i], length(defense_efficiencies)),
                    defoliation = defoliation) |>
    cbind(defol_list[[i]])
  # bind out to rest of dataframe:
  def_eff_defol_output <- rbind(def_eff_defol_output, out)
}


test <- def_eff_defol_output[def_eff_defol_output$def_eff == 0.25,]
plot(test$defoliation, test$Bleaf, type = "l", col = "navy", ylim = c(0,1))
lines(test$defoliation, test$Bleaf, col = "blue")
lines(test$defoliation, test$Bleaf, col = "purple")
lines(test$defoliation, test$Bleaf, col = "red")
#lines(abline(0,0.94), col = "black")
#lines(abline(-0.2, 0.91))

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
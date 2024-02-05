
# Base directory for the datasets
base_dir <- "/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/Dataset"

# List of dataset filenames (without the base directory)
filenames <- c(
  "gecco_sample.RData",
  "nab_sample.RData",
  "oil_3w_sample.RData",
  "rare_sample.RData",
  "ucr_sample.RData",
  "yahoo_sample.RData"
)

# Load the datasets
result_list_dt <- lapply(filenames, function(x) load(file.path(base_dir, x)))

gecco_sample <- result_list_dt[[1]]                                         


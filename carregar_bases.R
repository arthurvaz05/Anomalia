load_RData_files <- function(filenames, base_url) {
  loaded_data <- lapply(filenames, function(filename) {
    url <- paste0(base_url, "/", filename)
    temp_file <- tempfile(fileext = ".RData")
    download.file(url, temp_file, mode = "wb")  # Use binary mode for Windows
    
    loaded_object <- tryCatch(
      {
        load(temp_file, .GlobalEnv)
      },
      error = function(e) {
        warning(paste("Failed to load data from", url, "Error:", conditionMessage(e)))
        NULL  # Return NULL if loading fails
      }
    )
    
    unlink(temp_file)
    return(loaded_object)
  })
  
  return(loaded_data)
}

# Base URL for the datasets
base_url <- "https://raw.githubusercontent.com/arthurvaz05/Anomalia-sequencial/main/Dataset"

# List of dataset filenames (without the base URL)
filenames <- c(
  "gecco_sample.RData",
  "nab_sample.RData",
  "oil_3w_sample.RData",
  "rare_sample.RData",
  "ucr_sample.RData",
  "yahoo_sample.RData"
)

# Call the function to load the RData files
loaded_data <- load_RData_files(filenames, base_url)


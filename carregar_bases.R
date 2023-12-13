# Install package
install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/event_datasets", force = TRUE)
library(dalevents)

# List of dataset names
vetor_bases <- c("gecco", "yahoo_a1", "yahoo_a2", "yahoo_a3", "yahoo_a4",
                 "numenta_artificialNoAnomaly", "numenta_artificialWithAnomaly", 
                 "numenta_realAdExchange", "numenta_realAWSCloudwatch", "numenta_realKnownCause",
                 "numenta_realTraffic", "numenta_realTweets", "rare",
                 "ucr", "oil_3w_Type_0", "oil_3w_Type_1", "oil_3w_Type_2",
                 "oil_3w_Type_5", "oil_3w_Type_6", "oil_3w_Type_7", "oil_3w_Type_8",
                 "mortality_cnes", "fi_br", "eia_oil_prices")

# Load all datasets
loaded_datasets <- lapply(vetor_bases, function(dataset_name) {
  data(dataset_name)
  return(get(dataset_name))
})

# The loaded_datasets list now contains all the loaded datasets
loaded_datasets[1]

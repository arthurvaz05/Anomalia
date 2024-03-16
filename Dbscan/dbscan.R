source('/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/carregar_bases.R')


###LOAD AND INSTALL
{
  # Library to load and install if ins't already installed
  #install.packages('pacman')
  library(pacman)
  
  vetor_pacotes <- c("ggplot2", "plotly", "dplyr", "kableExtra", "gridExtra",
                     "TSPred", "dbscan", "fpc", "factoextra", "fitdistrplus",
                     "logspline", "caret","RColorBrewer","R.filesets","dbscan","fpc")
  
  pacman::p_load(char = vetor_pacotes)
  
  library(daltoolbox)
  library(harbinger)
}

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

load(file.path(base_dir,filenames[1]))


rm(result_list_dt)

run_dbscan <- function(sw, serie, minPts, data, data_name) {
  # Extract time series data with given sliding window size
  data_sw <- ts_data(data[, serie], sw)
  
  # Calculate kNN distances
  kNNdist <- kNNdist(data_sw, k = minPts)
  
  # Sort the distances in ascending order
  sorted_distances <- sort(kNNdist)
  
  # Calculate the percentage change of the sorted distances
  percentage_change <- c(0, diff(sorted_distances) / sorted_distances[-length(sorted_distances)] * 100)
  
  # Find the index of the maximum percentage change
  max_index <- which.max(percentage_change)
  eps <- sorted_distances[max_index]
  
  # Convert data into matrix for dbscan
  datamatrix <- matrix(data_sw, ncol = sw)
  
  # Perform DBSCAN clustering
  dbscanResult <- dbscan::dbscan(datamatrix, eps = eps, minPts = minPts)
  
  # Create a dataframe from datamatrix with clusters
  datamatrix_df <- data.frame(datamatrix)
  datamatrix_df$clusters <- dbscanResult$cluster
  
  # Filter anomalies
  datamatrix_df_anomalias <- datamatrix_df %>% filter(clusters == 0)
  
  # Extract indices of anomalies
  idx <- as.numeric(rownames(datamatrix_df_anomalias))
  
  # Condition to check if there are no anomalies, if so, return Null
  if (length(idx) == 0) {
    return()
  }
  
  # Create a dataframe with indices and event column equal to TRUE
  result <- data.frame(idx = idx, event = TRUE, seq = sw, variable = serie, dataset = data_name, eps = eps, minPts = minPts)
  
  # Set anomalia column in original data
  data$anomalia <- ifelse(1:nrow(data) %in% idx, 1, 0)
  
  # Create a contingency matrix
  contingency_matrix <- table(data$event, data$anomalia)
  
  # Rename the rows and columns of the contingency matrix
  rownames(contingency_matrix) <- c("Not Anomaly", "Anomaly")
  colnames(contingency_matrix) <- c("Not Event", "Event")
  
  # Extract true positives, false positives, and false negatives
  true_positives <- contingency_matrix["Anomaly", "Event"]
  false_positives <- contingency_matrix["Not Anomaly", "Event"]
  false_negatives <- contingency_matrix["Anomaly", "Not Event"]
  
  # Calculating recall
  recall <- true_positives / (true_positives + false_negatives)
  
  # Calculating precision
  precision <- true_positives / (true_positives + false_positives)
  
  # Calculating F1-score
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Adding recall, precision, and f1_score to result dataframe
  result$recall <- recall
  result$precision <- precision
  result$f1_score <- f1_score
  
  return(result)
}


for (j in c(5, 10, 15, 20, 25, 30)) {
  for (i in colnames(gecco_sample)[colnames(gecco_sample) != "event"]){
    
    result <- run_dbscan(sw = j, serie = i, minPts = 2, data = gecco_sample, data_name = 'gecco_sample')
    # conditional if df_final exists then rbind else create df_final
    if(exists("df_final")){
      df_final <- rbind(df_final, result)
    } else {
      df_final <- result
    }
  }
}



setwd('/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/Dbscan')
# SAVE the file
save(df_final, file = "Dbscan.RData")




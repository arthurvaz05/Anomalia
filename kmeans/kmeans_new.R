source('/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/carregar_bases.R')


###LOAD AND INSTALL
{
  # Library to load and install if ins't already installed
  #install.packages('pacman')
  library(pacman)
  
  vetor_pacotes <- c("ggplot2", "plotly", "dplyr", "kableExtra", "gridExtra",
                     "TSPred", "dbscan", "fpc", "factoextra", "fitdistrplus",
                     "logspline", "caret","RColorBrewer","R.filesets")
  
  pacman::p_load(char = vetor_pacotes)
  
  library(daltoolbox)
  library(harbinger)
  setwd('/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans')
  source('kmeans_anomalia_windows.R')
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


rm(df_final)

for (j in c(5, 10, 15, 20, 25, 30)) {
  for (i in colnames(gecco_sample)[colnames(gecco_sample) != "event"]){
    model <- hanct_kmeans(seq = j, centers = 2)
    fitted_model <- fit(model, gecco_sample[,i])
    detection <- detect(fitted_model, gecco_sample[,i])
    seq_window <- detection %>% filter(event == TRUE)  
    seq_window <- seq_window %>% mutate(variable = i)
    seq_window <- seq_window %>% mutate(dataset = "gecco_sample")
    
    # evaluating the detections
    evaluation <- evaluate(model, detection$event, gecco_sample[,'event'])
    
    seq_window <- seq_window %>% mutate(recall = evaluation$recall)
    seq_window <- seq_window %>% mutate(precision = evaluation$precision)
    seq_window <- seq_window %>% mutate(F1 = evaluation$F1)
    
    # conditional if df_final exists then rbind else create df_final
    if(exists("df_final")){
      df_final <- rbind(df_final, seq_window)
    } else {
      df_final <- seq_window
    }
  }
}

df_final

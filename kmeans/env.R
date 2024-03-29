#https://cran.r-project.org/web/packages/harbinger/harbinger.pdf
#https://github.com/cefet-rj-dal/harbinger-examples/blob/main/anomalies/hanct_kmeans_discord.ipynb
#https://github.com/cefet-rj-dal/harbinger/blob/master/R/


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


###. TEST ENV
{
  
  
}


###. GECCO - RESULTADOS KMEANS 5, 10, 15, 20, 25, 30
{
  result_list <- list()
  
  for (i in c(5, 10, 15, 20, 25, 30)) {
    result_list[[paste0("gecco", i)]] <- kmeans_windows_detected_optimized_test('gecco_sample', gecco_sample, i, '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas', save_file = FALSE)
  }
  gecco_df <- bind_rows(result_list)
  # reset index
  rownames(gecco_df) <- NULL
  # save the result as an RDS file
  saveRDS(gecco_df, file = '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas/gecco_windons.RDS')
}

###. YAHOO SAMPLE - RESULTADOS KMEANS 5, 10, 15, 20, 25, 30
{
  # Create a list to store the results
  result_list <- list()
  
  # Loop through each dataframe in the yahoo_sample list
  for (name_Dataset in names(yahoo_sample)) {
    dataset <- yahoo_sample[[name_Dataset]]
    
    # Specify the number of windows (e.g., 5, 10, 15, 20, 25, 30)
    for (i in c(5, 10, 15, 20, 25, 30)) {
      result_list[[paste0(name_Dataset, "_windows_", i)]] <- kmeans_windows_detected_optimized_test(name_Dataset, dataset, i, '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas', save_file = FALSE)
    }
  }
  
  # Combine the results into a single dataframe
  yahoo_df <- do.call(bind_rows, result_list)
  
  # Reset row names
  rownames(yahoo_df) <- NULL
  
  # Save the result as an RDS file
  saveRDS(yahoo_df, file = '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas/yahoo_sample_windows.RDS')
    
}

###. NAB SAMPLE - RESULTADOS KMEANS 5, 10, 15, 20, 25, 30
{
  # Create a list to store the results
  result_list <- list()
  
  # Loop through each dataframe in the yahoo_sample list
  for (name_Dataset in names(nab_sample)) {
    dataset <- nab_sample[[name_Dataset]]
    
    # Specify the number of windows (e.g., 5, 10, 15, 20, 25, 30)
    for (i in c(5, 10, 15, 20, 25, 30)) {
      result_list[[paste0(name_Dataset, "_windows_", i)]] <- kmeans_windows_detected_optimized_test(name_Dataset, dataset, i, '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas', save_file = FALSE)
    }
  }
  
  # Combine the results into a single dataframe
  nab_df <- do.call(bind_rows, result_list)
  
  # Reset row names
  rownames(nab_df) <- NULL
  
  # Save the result as an RDS file
  saveRDS(nab_df, file = '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas/nab_sample_windows.RDS')
  
}

###. OIL 3W SAMPLE - RESULTADOS KMEANS 5, 10, 15, 20, 25, 30
{
  # Create a list to store the results
  result_list <- list()
  
  # Loop through each dataframe in the yahoo_sample list
  for (name_Dataset in names(oil_3w_sample)) {
    dataset <- oil_3w_sample[[name_Dataset]]
    
    # Specify the number of windows (e.g., 5, 10, 15, 20, 25, 30)
    for (i in c(5, 10, 15, 20, 25, 30)) {
      result_list[[paste0(name_Dataset, "_windows_", i)]] <- kmeans_windows_detected_optimized_test(name_Dataset, dataset, i, '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas', save_file = FALSE)
    }
  }
  
  # Combine the results into a single dataframe
  oil_3w_df <- do.call(bind_rows, result_list)
  
  # Reset row names
  rownames(oil_3w_df) <- NULL
  
  # Save the result as an RDS file
  saveRDS(oil_3w_df, file = '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas/oil_3w_sample_windows.RDS')
  
}

###. UCR SAMPLE - RESULTADOS KMEANS 5, 10, 15, 20, 25, 30
{
  # Create a list to store the results
  result_list <- list()
  
  # Loop through each dataframe in the yahoo_sample list
  for (name_Dataset in names(ucr_sample)) {
    dataset <- ucr_sample[[name_Dataset]]
    
    # Specify the number of windows (e.g., 5, 10, 15, 20, 25, 30)
    for (i in c(5, 10, 15, 20, 25, 30)) {
      result_list[[paste0(name_Dataset, "_windows_", i)]] <- kmeans_windows_detected_optimized_test(name_Dataset, dataset, i, '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas', save_file = FALSE)
    }
  }
  
  # Combine the results into a single dataframe
  ucr_df <- do.call(bind_rows, result_list)
  
  # Reset row names
  rownames(ucr_df) <- NULL
  
  # Save the result as an RDS file
  saveRDS(ucr_df, file = '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas/ucr_sample_windows.RDS')
  
}

###. RARE SAMPLE - RESULTADOS KMEANS 5, 10, 15, 20, 25, 30
{
  # Create a list to store the results
  result_list <- list()
  
  # Loop through each dataframe in the yahoo_sample list
  for (name_Dataset in names(rare_sample)) {
    dataset <- rare_sample[[name_Dataset]]
    
    # Specify the number of windows (e.g., 5, 10, 15, 20, 25, 30)
    for (i in c(5, 10, 15, 20, 25, 30)) {
      result_list[[paste0(name_Dataset, "_windows_", i)]] <- kmeans_windows_detected_optimized_test(name_Dataset, dataset, i, '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas', save_file = FALSE)
    }
  }
  
  # Combine the results into a single dataframe
  rare_df <- do.call(bind_rows, result_list)
  
  # Reset row names
  rownames(rare_df) <- NULL
  
  # Save the result as an RDS file
  saveRDS(rare_df, file = '/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas/rare_sample_windows.RDS')
  
}



# Create a new environment
my_env <- new.env()

# Assuming my_env is the environment you want to "activate"
with(my_env, {
  source('/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/carregar_bases.R')
  
  ###LOAD AND INSTALL
  {
    # Library to load and install if ins't already installed
    #install.packages('pacman')
    library(pacman)
    
    vetor_pacotes <- c("ggplot2", "plotly", "dplyr", "kableExtra", "gridExtra",
                       "TSPred", "dbscan", "fpc", "factoextra", "fitdistrplus",
                       "logspline", "caret","RColorBrewer","R.filesets","dbscan","fpc","envir")
    
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
  
  library(reticulate)
  
  # Check which Python version is being used
  #use_python(Sys.which("python"))
  
  #py_install("matplotlib")
  #py_install("pandas")
  #py_install("torch")
  
  
  # dataset <- gecco_sample
  # serie <- 'ph'
  # # establishing han_autoencoder method 
  # model <- han_autoencoder(10,1)
  # # fitting the model
  # model <- fit(model, dataset[,serie])
  # # making detections using han_autoencoder
  # detection <- detect(model, dataset[,serie]) 
  # # filtering detected events
  # print(detection |> dplyr::filter(event==TRUE))
  # # evaluating the detections
  # evaluation <- evaluate(model, detection$event, dataset[,'event'])
  # print(evaluation$confMatrix)
  ## ploting the results
  #grf <- har_plot(model, dataset[,serie], detection, dataset[,'event'])
  #plot(grf)
  
  for (j in c(5, 10, 15)) {
    for (i in colnames(gecco_sample)[colnames(gecco_sample) != "event"]){
      model <- han_autoencoder(j,1)
      fitted_model <- fit(model, gecco_sample[,i])
      detection <- detect(fitted_model, gecco_sample[,i])
      seq_window <- detection %>% filter(event == TRUE)  
      seq_window <- seq_window %>% mutate(seq = j)
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
  
  # SAVE the file
  save(df_final, file = "/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/Autoencoder/autoencoder_result.RData")
  
  # Any functions called within this block will use objects from my_env
  print(ls())  # This will list the objects in my_env
})

# Load the file
load("autoencoder_result.RData")

# Assuming my_env is the environment you want to save
save(my_env, file = "/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/Autoencoder/autoencoder_env.RData")

###LOAD AND INSTALL
{
  # Library to load and install if ins't already installed
  #install.packages('pacman')
  library(pacman)
  
  vetor_pacotes <- c("ggplot2", "plotly", "dplyr", "kableExtra", "gridExtra",
                     "TSPred", "dbscan", "fpc", "factoextra", "fitdistrplus",
                     "logspline", "caret","RColorBrewer","changepoint")
  
  pacman::p_load(char = vetor_pacotes)
  
  load_library("daltoolbox") 
  load_library("harbinger")
  
  # Now you can use cpt.meanvar
  cpt.meanvar(cumsum(distancias), method = "BinSeg")
  
  
}

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter.R")
# Load the datasets
data("har_examples")

## introduzindo o binseg discretas

#load("har_examples.RData")
#dados = har_examples$example2
#dados = har_examples$example15
#dados = har_examples$example16
dados = har_examples$example17
#dados = har_examples$example18
#dados$series = rpois(100,lambda = 3)
#dados = rnbinom(100, size=10, prob=.5)



dbscan_dist_prob_ts <- function(vetor_serie, tamanho_janela, eps = NULL, minPts, limiar) {
  data_ts_janela <- ts_data(vetor_serie, tamanho_janela)
  datamatrix <- matrix(data_ts_janela, nrow = dim(data_ts_janela)[1], ncol = tamanho_janela)
  distancias <- kNNdist(data_ts_janela, k = minPts, all = TRUE)
  Binseg_cp <- cpt.meanvar(cumsum(distancias), method = "BinSeg", Q = 10)
  
  results <- list()
  
  for (i in 1:length(Binseg_cp@cpts)) {
    eps_value <- distancias[Binseg_cp@cpts[i]]
    dbscanResult <- dbscan(datamatrix, eps = eps_value, minPts = minPts)
    
    if (any(dbscanResult$cluster == 0)) {
      datamatrix_df <- data.frame(datamatrix)
      datamatrix_df$clusters <- dbscanResult$cluster
      
      datamatrix_df_anomalias <- datamatrix_df %>% filter(clusters == 0)
      datamatrix_df_anomalias <- subset(datamatrix_df_anomalias, select = -c(clusters))
      
      cat("Binseg change point for diff", i, "at location", Binseg_cp@cpts[i], "with value", eps_value, "\n")
      
      lista_dbscanresult[[i]] <- dbscanResult
      
      grafico_knn <- dbscan::kNNdistplot(datamatrix, k = minPts)
      abline(h = eps_value, lty = 2)
      
      grafico_boxplot <- boxplot(data.frame(t(datamatrix_df_anomalias)), plot = TRUE)
      
      grafico_dbscan <- dbscan::hullplot(datamatrix, dbscanResult, main = "DBSCAN")
      
      teste <- list()
      nomes_dist_bic <- character()
      
      for (j in 1:dim(datamatrix_df_anomalias)[1]) {
        dados_anomalia <- as.numeric(datamatrix_df_anomalias[j, ])
        fit_result <- fit_and_test_distribution(dados_anomalia)
        
        teste[[j]] <- fit_result$gof
        nomes_dist_bic[j] <- fit_result$best_fit
        
        probab_anomalias_bic[[j]] <- dados_anomalia[fit_result$is_anomaly]
      }
      
      lista_anomalias_unicas_detectadas[[i]] <- unique(unlist(probab_anomalias_bic))
      results[[i]] <- list(teste = teste, nomes_dist_bic = nomes_dist_bic)
    } else {
      cat("Não foram detectadas anomalias\n")
    }
  }
  
  return(results)
}

fit_and_test_distribution <- function(data) {
  fits <- list(
    fitdistrplus::fitdist(data, "lnorm"),
    fitdistrplus::fitdist(data, "exp"),
    fitdistrplus::fitdist(data, "gamma"),
    fitdistrplus::fitdist(data, "unif"),
    fitdistrplus::fitdist(data, "logis"),
    fitdistrplus::fitdist(data, "norm")
  )
  
  gof_stats <- lapply(fits, gofstat)
  best_fit_idx <- which.min(sapply(gof_stats, function(x) x$bic))
  best_fit <- names(fits)[best_fit_idx]
  
  is_anomaly <- gof_stats[[best_fit_idx]]$bic < limiar
  
  return(list(best_fit = best_fit, gof = gof_stats, is_anomaly = is_anomaly))
}

# Initialize your lists
teste <- list()
dados_anomalia <- list()
nomes_dist_bic <- list()
lista_nomes_dist_bic <- list()
probab_anomalias_bic <- list()
distancias <- matrix()
lista_anomalias_unicas_detectadas <- list()
lista_dbscanresult <- list()

# Call the function with your data
results <- dbscan_dist_prob_ts(dados, tamanho_janela = 19, minPts = 3, limiar = 0.10)

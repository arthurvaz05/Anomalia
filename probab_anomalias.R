probab <- function(datamatrix_df_anomalias, limiar, original_data) {
  results <- list()
  probab_anomalias_bic <- list()
  lista_anomalias_unicas_detectadas <- list()
  
  for (j in 1:dim(datamatrix_df_anomalias)[1]) {
    
    print(j)
    #######################################################
    
    # select all columns which contains "idx" in their names
    idx_cols <- grep("idx", names(datamatrix_df_anomalias), value = TRUE)
    result <- datamatrix_df_anomalias[j, idx_cols]
    # remove NA values
    result <- result[!is.na(result)]
    result <- as.numeric(result)
    
    dados_anomalia <- original_data[result,datamatrix_df_anomalias[j,'serie']]
    
    # remove NA values
    dados_anomalia <- dados_anomalia[!is.na(dados_anomalia)]
    
    #######################################################
    
    # Filter out zero values
    dados_anomalia <- dados_anomalia[dados_anomalia != 0] 
    
    if (length(dados_anomalia) <= 1) {
      # If data has only one value, return a result indicating it's not suitable
      results[[j]] <- list(
        best_fit = "Not Suitable",
        gof = NULL,
        is_anomaly = FALSE
      )
    } else {
      fit_result <- fit_and_test_distribution(dados_anomalia, limiar)
      results[[j]] <- list(
        teste = fit_result$gof,
        nomes_dist_bic = fit_result$best_fit
      )
      probab_anomalias_bic[[j]] <- dados_anomalia[fit_result$is_anomaly]
    }
  }
  
  lista_anomalias_unicas_detectadas[[j]] <- unique(unlist(probab_anomalias_bic))
  
  return(list(results,probab_anomalias_bic,lista_anomalias_unicas_detectadas))
}

library(dplyr)
library(fitdistrplus)

fit_and_test_distribution <- function(data, limiar) {
  print(data)
  
  fits <- list()
  
  distribution_list <- c("gamma", "lnorm", "exp", "gamma", "unif", "logis", "norm")
  
  for (dist in distribution_list) {
    tryCatch({
      fits[[dist]] <- fitdist(data, dist)
    }, error = function(e) {
      cat("Error fitting distribution", dist, ": ", conditionMessage(e), "\n")
    })
  }
  
  gof_stats <- lapply(fits, gofstat)
  
  best_fit_idx <- which.min(sapply(gof_stats, function(x) x$bic))
  best_fit <- names(best_fit_idx)
  
  if (is.null(best_fit)) {
    fits <- list(best_fit = "Not Suitable",gof = NULL,is_anomaly = FALSE, NULL)
    return(fits)
  } else {
    best_fit <- strsplit(best_fit, "-")[[1]][3]
  }
  
  if (best_fit %in% c("lnorm", "exp", "gamma", "unif", "logis")) {
    params <- as.list(fits[[best_fit_idx]]$estimate)
    is_anomaly <- switch(
      best_fit,
      lognormal = dlnorm(data, meanlog = params$meanlog, sdlog = params$sdlog) < limiar,
      exp = dexp(data, rate = params$rate) < limiar,
      gamma = dgamma(data, shape = params$shape, rate = params$rate) < limiar,
      unif = dunif(data, min = params$min, max = params$max) < limiar,
      logis = dlogis(data, location = params$location, scale = params$scale) < limiar
    )
    anomalias <- data[is_anomaly]
  } else {
    is_anomaly <- dnorm(data, mean = fits[[best_fit_idx]]$estimate[['mean']], sd = fits[[best_fit_idx]]$estimate[['sd']]) < limiar
    anomalias <- data[is_anomaly]
  }
  
  return(list(best_fit = best_fit, gof = gof_stats, is_anomaly = is_anomaly, anomalias))
}

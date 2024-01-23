probab <- function(datamatrix_df_anomalias, limiar, original_data) {
  results <- list()
  
  for (j in 1:nrow(datamatrix_df_anomalias)) {
    print(j)
    
    idx_cols <- grep("idx", names(datamatrix_df_anomalias), value = TRUE)
    result <- as.numeric(datamatrix_df_anomalias[j, idx_cols][!is.na(datamatrix_df_anomalias[j, idx_cols])])
    
    dados_anomalia <- original_data[result, datamatrix_df_anomalias[j, 'serie']]
    dados_anomalia <- dados_anomalia[!is.na(dados_anomalia)]
    dados_anomalia <- dados_anomalia[dados_anomalia != 0] 
    
    if (length(dados_anomalia) > 1) {
      fit_result <- fit_and_test_distribution(dados_anomalia, limiar)
      
      if (any(fit_result$is_anomaly)) {
        result_item <- list(
          estatistica = fit_result$gof,
          nomes_dist_bic = fit_result$best_fit
        )
        probab_anomalias_bic_item <- dados_anomalia[fit_result$is_anomaly]
        lista_anomalias_unicas_detectadas_item <- unique(unlist(probab_anomalias_bic_item))
        idx_list_item <- result
        
        # Create labels for each sublist
        label <- paste("Result for row", j)
        results[[label]] <- result_item
        probab_anomalias_bic[[label]] <- probab_anomalias_bic_item
        lista_anomalias_unicas_detectadas[[label]] <- lista_anomalias_unicas_detectadas_item
        idx_list[[label]] <- idx_list_item
      }
    }
  }
  
  return(list(results = results, probab_anomalias_bic = probab_anomalias_bic, lista_anomalias_unicas_detectadas = lista_anomalias_unicas_detectadas, idx_list = idx_list))
}
library(dplyr)
library(fitdistrplus)

fit_and_test_distribution <- function(data, limiar) {
  print(data)
  
  distribution_list <- c("gamma", "lnorm", "exp", "gamma", "unif", "logis", "norm")
  fits <- list()
  
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
    fits <- list(
      best_fit = "Not Suitable",
      gof = NULL,
      is_anomaly = FALSE,
      anomalias = numeric(0)
    )
    return(fits)
  } else {
    best_fit <- strsplit(best_fit, "-")[[1]][3]
  }
  
  params <- as.list(fits[[best_fit_idx]]$estimate)
  is_anomaly <- switch(
    best_fit,
    lognormal = dlnorm(data, meanlog = params$meanlog, sdlog = params$sdlog) < limiar,
    exp = dexp(data, rate = params$rate) < limiar,
    gamma = dgamma(data, shape = params$shape, rate = params$rate) < limiar,
    unif = dunif(data, min = params$min, max = params$max) < limiar,
    logis = dlogis(data, location = params$location, scale = params$scale) < limiar,
    dnorm(data, mean = params$mean, sd = params$sd) < limiar
  )
  
  anomalias <- data[is_anomaly]
  return(list(best_fit = best_fit, best_fit_idx = best_fit_idx, gof = gof_stats, is_anomaly = is_anomaly, anomalias = anomalias))
}


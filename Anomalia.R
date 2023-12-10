###LOAD AND INSTALL
{
  # Library to load and install if ins't already installed
  #install.packages('pacman')
  library(pacman)
  
  vetor_pacotes <- c("ggplot2", "plotly", "dplyr", "kableExtra", "gridExtra",
                     "TSPred", "dbscan", "fpc", "factoextra", "fitdistrplus",
                     "logspline", "caret","RColorBrewer")
  
  pacman::p_load(char = vetor_pacotes)
  
  load_library("daltoolbox") 
  load_library("harbinger")
  
  
}



source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter.R")

# Load the datasets
data("har_examples")


# Select the time series 17
dados <- har_examples[[17]]
head(dataset)
# Making sure if the event column is factor
dados$event = factor(dados$event, labels=c("FALSE", "TRUE"))

# Total outliers
print(paste('Total outliers:', sum(dados$event == "TRUE")))

# Plot the time series
plot_ts(x = 1:length(dados$serie), y = dados$serie )

# Adicionando um índice aos dados
dados$indice <- 1:length(dados$serie)

# Criando uma série temporal com os dados
my_ts <- ts(dados$serie, start = dados$indice[1], end = dados$indice[length(dados$indice)])

# Plotando a série temporal
plot(my_ts, main = "Série Temporal", xlab = "Índice", ylab = "Valor da Série")

# Plotando a série temporal com outlier
{
  # Certifique-se de que `dados$event` é um vetor lógico
  dados$event <- dados$event == "TRUE"
  # Criando uma série temporal com os dados
  my_ts <- ts(dados$serie, start = min(dados$indice), end = max(dados$indice))
  # Plotando a série temporal com outliers destacados
  plot(my_ts, main = "Série Temporal", xlab = "Índice", ylab = "Valor da Série")
  points(which(dados$event), dados$serie[dados$event], pch = 20, col = "red")  
}

# Parametros para janela deslizante
janelas_n = 10

# EPS escolha
{
  #Referencias
  #____https://cdn.aaai.org/KDD/1996/KDD96-037.pdf
  #____https://iopscience.iop.org/article/10.1088/1755-1315/31/1/012012/pdf
  
  # Metodo 1
  {
    data10 <- ts_data(dados$serie, janelas_n)
    datamatrix <- matrix(data10,nrow = 104,ncol = 10)
    
    dbscan::kNNdistplot(datamatrix, k =  3)
    abline(h = 0.53, lty = 2)  
  }
  # Metodo 2
  {
    # Calculando as distâncias k-NN
    k_dist <- sort(kNNdist(datamatrix, k = 3))
    
    # Calculando a taxa de mudança das distâncias k-NN
    rate_of_change <- diff(k_dist)
    
    # Encontrando o índice do maior aumento na taxa de mudança
    max_increase_index <- which.max(rate_of_change)
    
    # O valor de 'eps' é escolhido como a distância k-NN no ponto de maior aumento
    eps_estimado <- k_dist[max_increase_index]
    
    # Plotando para verificar visualmente
    plot(k_dist, type = "b", main = "3-NN Distance Plot",
         xlab = "Points (sorted by distance)", ylab = "3-NN Distance")
    abline(h = eps_estimado, col = "red", lty = 2) 
    
    cat("Valor de Eps estimado baseado na estimativa de ruído:", eps_estimado, "\n")
    
  }
  # Metodo 3
  {
    # 1. Gerar o gráfico 3-dist
    k_dist <- sort(kNNdist(datamatrix, k = 3))
    plot(k_dist, type = "b", main = "3-NN Distance Plot",
         xlab = "Points (sorted by distance)", ylab = "3-NN Distance")
    
    # 2. Derivar uma proposta para o ponto de limite a partir da estimativa de ruído
    # Suponha que o usuário forneceu uma estimativa de 10% de ruído
    percentual_ruido <- 10
    ponto_sugerido <- quantile(k_dist, probs = 1 - percentual_ruido/100)
    
    # 3. Permitir que o usuário selecione o ponto de corte
    # O usuário pode aceitar o ponto sugerido ou selecionar outro ponto manualmente
    # Aqui está um exemplo de como aceitar a sugestão automaticamente
    eps_estimado <- ponto_sugerido
    
    # Adicionar uma linha no gráfico para o valor de eps estimado
    abline(h = eps_estimado, col = "red", lty = 2)
    
    # Informar o valor de eps estimado
    cat("Valor de Eps estimado baseado na estimativa de ruído:", eps_estimado, "\n")
  }
}

# Clusters DBSCAN e grafico 1
{
  dbscanResult <- dbscan::dbscan(datamatrix, eps=0.53 , minPts=3) # clustering
  
  dbscanResult
  
  # O número de cores deve ser pelo menos igual ao número de clusters mais um para o ruído
  num_clusters <- max(dbscanResult$cluster) + 1  # Adicione 1 para o ruído
  color_palette <- brewer.pal(ifelse(num_clusters <= 8, num_clusters, 8), "Set1")
  
  # Se houver mais clusters do que cores na paleta 'Set1', crie uma paleta de cores maior
  if (num_clusters > 8) {
    color_palette <- colorRampPalette(brewer.pal(8, "Set1"))(num_clusters)
  }
  
  # Plote os clusters usando a paleta de cores definida
  hullplot(datamatrix, dbscanResult, main="DBSCAN", col = color_palette)
  
}

# Visualizacao grafica 2
{
  db <- fpc::dbscan(datamatrix, eps= 0.53, MinPts =3)
  print(db)
  plot(db, datamatrix, main = "DBSCAN", frame = FALSE)
}

# Visualizacao grafica 3
{
  fviz_cluster(db, datamatrix, stand = FALSE, ellipse = FALSE, geom = "point")
}

# Clusters com indicacao de anomalia pelo DBSCAN
{
  datamatrix_df = data.frame(datamatrix)
  
  datamatrix_df$clusters =  dbscanResult$cluster
  
  datamatrix_df_anomalias = datamatrix_df %>% filter(clusters==0)
  datamatrix_df_anomalias  
  
  datamatrix_df_anomalias_t = data.frame(t(datamatrix_df_anomalias))
  datamatrix_df_anomalias_t = datamatrix_df_anomalias_t[-11,]
  datamatrix_df_anomalias_t
  
}

# Boxplot
{
  graphics::boxplot(datamatrix_df_anomalias_t)
}

#### FUNCOES: MEAN, MEDIAN e LOG
{
  max_mean = function(dados)  {
    a = which.max(abs(dados - mean(dados)))
    b = dados[a]
    #c = list(a,b)
    return( b )
  }
  max_median = function(dados)  {
    a = which.max(abs(dados - median(dados)))
    b = dados[a]
    #c = list(a,b)
    return( b )
  }
  max_log = function(dados)  {
    a = which.max(log(abs(dados) - median(dados)))
    b = dados[a]
    #c = list(a,b)
    return( b )
  }
  # Aplica uma função a cada coluna do dataframe e obtém os valores únicos
  get_unique_values <- function(df, func) {
    unique(apply(df, 2, func))
  }
}

# Calculando por MEAN, MEDIAN e LOG
{
  list_v_max_mean <- get_unique_values(datamatrix_df_anomalias_t, max_mean)
  list_v_max_median <- get_unique_values(datamatrix_df_anomalias_t, max_median)
  list_v_max_log <- get_unique_values(datamatrix_df_anomalias_t, max_log)
  
  # Exibindo os resultados
  list_v_max_mean
  list_v_max_median
  list_v_max_log
}

# Comparando os resultados
{
  # Definição prévia do vetor de eventos verdadeiros
  vetor_evento <- dados$serie[dados$event == "TRUE"]
  
  # Função otimizada para comparar anomalias
  compara_anomalias <- function(dados1, dados2) {
    # Arredondamento e obtenção de valores únicos apenas uma vez
    dados1_uniq <- unique(round(dados1, 6))
    dados2_uniq <- unique(round(dados2, 6))
    
    # Verificação de correspondência
    matches <- dados1_uniq %in% dados2_uniq
    
    # Construção da lista de resultados
    list(
      matches = matches,
      count = table(matches),
      false_positives = dados1_uniq[!matches],
      true_positives = dados1_uniq[matches]
    )
  }
  
  # Aplicação da função compara_anomalias aos vetores de anomalias
  results <- lapply(list(list_v_max_mean, list_v_max_median, list_v_max_log), compara_anomalias, dados2 = vetor_evento)
  
  # Acessando os resultados
  c1 <- results[[1]]
  c2 <- results[[2]]
  c3 <- results[[3]]
}

# Plotando no grafico e comparando os resultados
{
  plot_anomalias_unico_grafico <- function(my_ts, dados, results_list) {
    # Cores para cada conjunto de resultados
    cores <- c("red", "blue", "green")
    
    # Plotar a série temporal base
    plot(my_ts, main = "Anomalias por Método", ylab = "Valor da Série")
    
    for (i in seq_along(results_list)) {
      # Determinar os pontos de anomalia para cada conjunto de resultados
      true_positives <- results_list[[i]]$true_positives
      dados$event_anomalia <- dados$serie %in% true_positives
      
      # Adicionar pontos de anomalia
      points(dados$indice[dados$event_anomalia], dados$serie[dados$event_anomalia], pch = 20, col = cores[i])
    }
    
    # Adicionar legenda com tamanho de texto reduzido e no canto inferior direito
    legend("bottomright", legend = c("Max Mean", "Max Median", "Max Log"), col = cores, pch = 20, cex = 0.75)
  }
  
  # Usar a função com my_ts, dados e a lista de resultados (c1, c2, c3)
  plot_anomalias_unico_grafico(my_ts, dados, list(c1, c2, c3))
  
}

# Tabela comparativa
{
  dados_comparativos_DBSCAN = dados %>% filter(event == "TRUE")
  dados_comparativos_DBSCAN  %>% 
    kbl() %>%
    kable_styling()
}


# Versao final do código DBSCAN e distribuicao
{
  # Load the datasets
  data("har_examples")
  
  # Select the time series 17
  dados <- har_examples[[17]]
  # Making sure if the event column is factor
  dados$event = factor(dados$event, labels=c("FALSE", "TRUE"))
  
  # Função para calcular as probabilidades usando DBSCAN
  dbscan_dist_prob_ts <- function(vetor_serie, tamanho_janela, eps, minPts, limiar) {
    data_ts_janela <- ts_data(vetor_serie, tamanho_janela)
    datamatrix <- matrix(data_ts_janela, nrow = dim(data_ts_janela)[1], ncol = tamanho_janela)
    
    # Executando DBSCAN
    dbscanResult <- dbscan::dbscan(datamatrix, eps = eps, minPts = minPts)
    datamatrix_df <- data.frame(datamatrix)
    datamatrix_df$clusters <- dbscanResult$cluster
    
    # Filtrando anomalias
    datamatrix_df_anomalias <- datamatrix_df %>% filter(clusters == 0)
    
    probab_anomalias_bic <- list()
    
    for (i in 1:dim(datamatrix_df_anomalias)[1]) {
      fit_results <- list()
      
      for (dist_name in c("lnorm", "exp", "gamma", "unif", "logis", "norm")) {
        fit <- try(fitdistrplus::fitdist(as.numeric(datamatrix_df_anomalias[i, ]), dist_name, start = list(mean = 0, sd = 1)), silent = TRUE)
        
        if (!inherits(fit, "try-error")) {
          fit_results[[dist_name]] <- fit
        }
      }
      
      if (length(fit_results) > 0) {
        gof_stats <- sapply(fit_results, function(fit) {
          bic <- -2 * fit$loglik + log(length(datamatrix_df_anomalias[i, ])) * (length(fit$estimate) + 1)
          return(bic)
        })
        
        best_fit_name <- names(which.min(gof_stats))
        best_fit <- fit_results[[best_fit_name]]
        
        dados_anomalia <- as.numeric(datamatrix_df_anomalias[i, ])
        
        if (best_fit_name == "lnorm") {
          probab_anomalias_bic[[i]] <- dados_anomalia[dlnorm(dados_anomalia, meanlog = best_fit$estimate[[1]], sdlog = best_fit$estimate[[2]]) < limiar]
        } else if (best_fit_name == "exp") {
          probab_anomalias_bic[[i]] <- dados_anomalia[dexp(dados_anomalia, rate = best_fit$estimate[[1]]) < limiar]
        } else if (best_fit_name == "gamma") {
          probab_anomalias_bic[[i]] <- dados_anomalia[dgamma(dados_anomalia, shape = best_fit$estimate[[1]], rate = best_fit$estimate[[2]]) < limiar]
        } else if (best_fit_name == "unif") {
          probab_anomalias_bic[[i]] <- dados_anomalia[dunif(dados_anomalia, min = best_fit$estimate[[1]], max = best_fit$estimate[[2]]) < limiar]
        } else if (best_fit_name == "logis") {
          probab_anomalias_bic[[i]] <- dados_anomalia[dlogis(dados_anomalia, location = best_fit$estimate[[1]], scale = best_fit$estimate[[2]]) < limiar]
        } else if (best_fit_name == "norm") {
          probab_anomalias_bic[[i]] <- dados_anomalia[dnorm(dados_anomalia, mean = best_fit$estimate[[1]], sd = best_fit$estimate[[2]]) < limiar]
        }
      }
    }
    
    vetor_anomalias_unicas_detectadas <- unique(unlist(probab_anomalias_bic))
    
    # Gráficos (não otimizados)
    grafico_dbscan <- plot_hull(datamatrix, dbscanResult, main = "DBSCAN")
    grafico_boxplot <- boxplot(data.frame(t(datamatrix_df_anomalias)), plot = TRUE)
    grafico_knn <- dbscan::kNNdistplot(datamatrix, k = minPts)
    
    result <- list(
      probab_anomalias_bic,
      vetor_anomalias_unicas_detectadas,
      dados_anomalia,
      dbscanResult
    )
    
    return(result)
  }
  
  # Função para plotar o gráfico de hull
  plot_hull <- function(datamatrix, dbscanResult, main = "DBSCAN") {
    n_clusters <- max(dbscanResult$cluster)
    colors <- rainbow(n_clusters)
    
    plot(NULL, xlim = range(datamatrix[, 1]), ylim = range(datamatrix[, 2]), xlab = "X", ylab = "Y", main = main)
    
    for (i in 1:n_clusters) {
      cluster_points <- datamatrix[dbscanResult$cluster == i, ]
      polygon(cluster_points, col = colors[i], border = colors[i], lwd = 2)
    }
    
    legend("topright", legend = as.character(1:n_clusters), fill = colors)
  }
  
  # Chamando a função
  result <- dbscan_dist_prob_ts(dados$serie, tamanho_janela = 10, eps = 0.53, minPts = 3, limiar = 0.25)
  
  # Verificando anomalias em relação aos eventos reais
  event_detectados <- table(result[[2]] %in% dados$serie[dados$event == TRUE])
  
  # Outras operações (não otimizadas)
  c1 <- compara_anomalias(result[[2]], dados$serie)
  dados$event_dbscan_dist_prob_ts <- dados$serie %in% c1[[4]]
  dados$indice <- 1:length(dados$serie)
  my_ts <- ts(dados$serie, dados$indice)
  
  # Gráfico (não otimizado)
  plot.ts(my_ts)
  points(dados$indice[dados$event == "TRUE"], dados$serie[dados$event == "TRUE"], pch = 20, col = "red")
  
  plot.ts(my_ts)
  points(dados$indice[dados$event_dbscan_dist_prob_ts == "TRUE"], dados$serie[dados$event_dbscan_dist_prob_ts], pch = 20, col = "red")
  
}


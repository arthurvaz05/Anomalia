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

# SAVE the file
save(df_final, file = "kmeans_result.RData")

# Load the file
load("kmeans_result.RData")
df_final%>%head()

############-----Analysis-----############

# Number of windows detected sorted by variable
df_final %>% group_by(variable) %>% summarise(n = n()) %>% arrange(desc(n))

# Number of sizes of windows detected
df_final %>% group_by(seq) %>% summarise(n = n()) %>% arrange(desc(n))

# Average recall and precision by variable
df_final %>% group_by(variable) %>% summarise(mean_recall = mean(recall), mean_precision = mean(precision)) %>% arrange(desc(mean_precision))

# Average recall and precision by size
df_final %>% group_by(seq) %>% summarise(mean_recall = mean(recall), mean_precision = mean(precision)) %>% arrange(desc(mean_precision))

# Convert seqlen to a factor
df_final$seqlen <- factor(df_final$seqlen)

# Check unique values in the seqlen variable
unique_seqlen <- unique(df_final$seqlen)

# Determine the number of unique seqlen values
num_unique_seqlen <- length(unique_seqlen)

# Create a scatter plot using scale_shape_manual
scatter_plot <- ggplot(df_final, aes(x = recall, y = precision, color = variable, shape = seqlen)) +
  geom_point() +
  labs(x = "Recall", y = "Precision", color = "Variable", shape = "Seqlen") +
  scale_shape_manual(values = 1:num_unique_seqlen) +  # Assign shapes manually
  ggtitle("Gecco Kmeans result: Precision vs. Recall") +  # Add a title
  theme_minimal()

# Show the scatter plot
print(scatter_plot)





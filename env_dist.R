
setwd("/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/kmeans/janelas")
gecco_result <- readRDS("gecco_windons.RDS")
setwd("/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/Dataset")
load("gecco_sample.RData")

source('/Users/arthurvaz/Desktop/CEFETRJ - Mestrado/Anomalia/Anomalia/probab_anomalias.R')


resultado <- probab(gecco_result,0.2,gecco_sample)


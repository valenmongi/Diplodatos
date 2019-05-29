#  Dataset in https://archive.ics.uci.edu/ml/datasets/Wine

set.seed(0)
data=read.table("wine.data",sep=",")
data_sc=data
#  Funcion de normalizacion con valores maximos y minimos

normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

#  Normalizacion de las variables, excepto las clases

data[,2:14]=lapply(data[,2:14],normalize)
data_sc[,2:14]=lapply(data_sc[,2:14],scale)

# K MEANS

k=c(1,3,5,8,11,15,18,21,25) # Vector con los distintos k a probar

for (i in k){
  
  dataCluster=kmeans(data[,2:14],3,nstart=i)
  dataCluster_sc=kmeans(data_sc[,2:14],3,nstart=i)
  
  cat("k=",i,"\n")
  cat("\n")
  cat("Norm Max Min")
  print(table(dataCluster$cluster,data[,1]))
  cat("Norm Scale")
  print(table(dataCluster_sc$cluster,data_sc[,1]))
}



# Mixturas Gaussianas

mcl.model <- Mclust(data[, c(1:14)], 3)
mcl.model_sc <- Mclust(data_sc[, c(1:14)], 3)


table(mcl.model$classification,data[,1])

table(mcl.model_sc$classification,data_sc[,1])


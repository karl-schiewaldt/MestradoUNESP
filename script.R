library(tidyverse)      #pacote para manipulacao de dados
library(cluster)        #algoritmo de cluster
library(dendextend)     #compara dendogramas
library(factoextra)     #algoritmo de cluster e visualizacao
library(fpc)            #algoritmo de cluster e visualizacao
library(gridExtra)      #para a funcao grid arrange
library(psych)

### CARREGA OS DADOS
data_uhf <- R.matlab::readMat('uhf_data_r.mat')
data_acust <- R.matlab::readMat('acustico_data_r.mat')

df_uhf <- data.frame(t(data_uhf$data.UHF))
df_acust <- data.frame(t(data_acust$data.Acustico))

Fs_uhf <- data_uhf$Fs[1]
Fs_acust <- data_acust$Fs[1]

### AJUSTANDO OS DADOS
lista_uhf <- c()
lista_acust <- c()

# lista variaveis - uhf
for(i in 1:ncol(df_uhf)){
  if(i <= 100) {
    lista_uhf <- append(lista_uhf, paste0('desc_bucha', i))
  }
  if(i >= 101 & i <= 200) {
    lista_uhf <- append(lista_uhf, paste0('curto_circ', i-100))
  }
  if(i >= 201) {
    lista_uhf <- append(lista_uhf, paste0('ruido', i-200))
  }
}

# lista variaveis - acustico
for(i in 1:ncol(df_acust)){
  if(i <= 97) {
    lista_acust <- append(lista_acust, paste0('desc_bucha', i))
  }
  if(i >= 98 & i <= 194) {
    lista_acust <- append(lista_acust, paste0('curto_circ', i-97))
  }
  if(i >= 195) {
    lista_acust <- append(lista_acust, paste0('ruido', i-194))
  }
}
rm(i)

#renomeia variaveis
colnames(df_uhf) <- lista_uhf
colnames(df_acust) <- lista_acust
rm(lista_uhf)
rm(lista_acust)

#train-test split
sample <- sample(c(TRUE, FALSE), nrow(df_uhf), replace = TRUE, prob = c(0.7,0.3))
train  <- df_uhf[sample, ]
test   <- df_uhf[!sample, ]


### K-MEANS
# número de clusters
k <- 3

# constrói k-means
fviz_nbclust(df_uhf, FUN = hcut, method = "wss")
df_uhf.k3 <- kmeans(df_uhf, centers = k)
fviz_cluster(df_uhf.k3, geom = 'point', data = df_uhf)

df_uhf_agrup <- df_uhf
df_uhf_agrup$grupo <- df_uhf.k3$cluster



### PCA
df_uhf_desc <- df_uhf[, 1:100]
df_uhf_desc_cor <- cor(df_uhf_desc)
df_uhf_desc_std <- scale(df_uhf_desc)

#bartlett: Se o teste for significativo, indica que a base de dados apresenta 
#uma estrutura multivariada que justifica a aplicação da PCA
#p-value < 0.05 (5% de significância)
cortest.bartlett(df_uhf_desc_cor)
bartlett.test(df_uhf_desc)

#KMO: avalia a adequação da base de dados para a PCA, levando em conta 
#a correlação entre as variáveis e a adequação da amostra. Um valor de KMO 
#próximo a 1 indica uma alta adequação para a PCA.
KMO(df_uhf_desc)

#aplica PCA
pca_desc <- prcomp(df_uhf_desc_std, scale. = FALSE)
summary(pca_desc)

comp_desc <- data.frame(pca_desc$x[, 1:3])

# script para instalacao dos pacotes necessarios para o 
# minicurso Aprendizado de Máquina com o Pacote `tidymodels`.
# este script encontra os pacotes instalados na maquina 
# do usuario e baixa e instala apenas aqueles que estao
# faltando.
# 
# autor: Marcus Nunes 
# site:  https://marcusnunes.me

# repositorio de pacotes

options(repos = c(CRAN = "http://cran.rstudio.com"))

# lista de pacotes necessarios

pacotes.necessarios <- c("e1071", 
                         "GGally", 
                         "ggfortify", 
                         "janitor", 
                         "kknn", 
                         "onehot", 
                         "palmerpenguins", 
                         "pROC", 
                         "randomForest", 
                         "rvest", 
                         "scales", 
                         "themis", 
                         "tidymodels", 
                         "tidyverse",
                         "vip")

# instalacao dos pacotes que faltam na maquina

pacotes.novos <- pacotes.necessarios[!(pacotes.necessarios %in% installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(pacotes.novos, dependencies = TRUE)
  print("##########################")
  print("### Pacotes instalados ###")
  print("##########################")
} 

# atualizacao dos pacotes jah instalados

update.packages(ask = FALSE)

print("###########################")
print("### Pacotes atualizados ###")
print("###########################")

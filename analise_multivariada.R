##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats, stringi, corrplot, leaps)
################################

# Carregar os dados
dados <- read.table(text="Ano Desmatamento_ha Volume Cambio Rebanho_bovino PIB
2010 700000 10687431.07 1.7603 77.4 387600000000
2011 641800 13656931.12 1.6750 79.3 429200000000
2012 457100 12516534.98 1.9546 79.6 458700000000
2013 589100 11946988.91 2.1576 80.3 479100000000
2014 501200 12775974.1 2.3534 81.75 502300000000
2015 620700 10476359.58 3.3315 83.75 525400000000
2016 789300 9019802.371 3.4901 85.50 541800000000
2017 694700 9492633.366 3.1920 85.55 562000000000
2018 753600 9832579.75 3.6542 86.24 589200000000
2019 1012900 9786855.766 3.9451 89.21 614500000000
2020 1085100 8576891.835 5.1558 93.0 623300000000
2021 1303800 10214760.35 5.3950 96.0 645000000000
2022 1159400 10260838.3 5.1648 104.3 670200000000
2023 900100 6057240.671 4.9951 107.0 694100000000", header=TRUE)

# Verificar a correlação entre as variáveis
correlacao <- cor(dados[, -1]) # Excluir a primeira coluna (Ano)
print(correlacao)

# Realizar a análise de regressão múltipla
modelo <- lm(Desmatamento_ha ~ Volume + Cambio + Rebanho_bovino + PIB, data=dados)
summary(modelo)

# Plotar a matriz de correlação - Teste 1
library(corrplot) # Carregar o pacote corrplot
corrplot(correlacao, method = "circle", type = "lower", order = "hclust", tl.cex = 0.5)

# Plotar a matriz de correlação com destaque para as diferenças - teste 2
corrplot(correlacao, method = "color", type = "lower", order = "hclust", 
         tl.cex = 0.5, tl.col = "black", col = colorRampPalette(c("darkgreen", "white", "brown"))(200),
         number.cex = 0.6, addrect = 4)



# Realizar seleção de características usando Forward Selection - Ver se o modelo é bom
modelo_forward <- regsubsets(Desmatamento_ha ~ Volume + Cambio + Rebanho_bovino + PIB, data=dados, method="forward")
summary(modelo_forward)

# Aplicar transformação logarítmica aos dados
dados$log_Desmatamento_ha <- log(dados$Desmatamento_ha)
dados$log_Volume <- log(dados$Volume)
dados$log_Cambio <- log(dados$Cambio)
dados$log_Rebanho_bovino <- log(dados$Rebanho_bovino)
dados$log_PIB <- log(dados$PIB)

# Realizar análise de regressão múltipla com variáveis transformadas
modelo_log <- lm(log_Desmatamento_ha ~ log_Volume + log_Cambio + log_Rebanho_bovino + log_PIB, data = dados)
summary(modelo_log)

# Calcular a matriz de correlação para as variáveis em log
correlacao_log <- cor(dados[c("log_Desmatamento_ha", "log_Volume", "log_Cambio", "log_Rebanho_bovino", "log_PIB")])

# Imprimir a matriz de correlação
print(correlacao_log)

# Plotar a matriz de correlação com destaque para as diferenças - teste 2
corrplot(correlacao_log, method = "color", type = "lower", order = "hclust", 
         tl.cex = 0.5, tl.col = "black", col = colorRampPalette(c("darkgreen", "white", "brown"))(200),
         number.cex = 0.6, addrect = 4)

# Aplicar transformação exponencial aos dados
dados$exp_Desmatamento_ha <- exp(dados$log_Desmatamento_ha)
dados$exp_Volume <- exp(dados$log_Volume)
dados$exp_Cambio <- exp(dados$log_Cambio)
dados$exp_Rebanho_bovino <- exp(dados$log_Rebanho_bovino)
dados$exp_PIB <- exp(dados$log_PIB)

# Verificar as transformações
print(dados[c("exp_Desmatamento_ha", "exp_Volume", "exp_Cambio", "exp_Rebanho_bovino", "exp_PIB")])

# Realizar análise de regressão múltipla com variáveis transformadas
modelo_exp <- lm(exp_Desmatamento_ha ~ exp_Volume + exp_Cambio + exp_Rebanho_bovino + exp_PIB, data = dados)
summary(modelo_exp)

# Calcular a matriz de correlação para as variáveis em log
correlacao_exp <- cor(dados[c("exp_Desmatamento_ha", "exp_Volume", "exp_Cambio", "exp_Rebanho_bovino", "exp_PIB")])

# Imprimir a matriz de correlação
print(correlacao_exp)

# Plotar a matriz de correlação com destaque para as diferenças - teste 2
corrplot(correlacao_exp, method = "color", type = "lower", order = "hclust", 
         tl.cex = 0.5, tl.col = "black", col = colorRampPalette(c("darkgreen", "white", "brown"))(200),
         number.cex = 0.6, addrect = 4)

# Aplicar transformação polinomial às variáveis
dados$Volume_quad <- dados$Volume^2
dados$Cambio_quad <- dados$Cambio^2
dados$Rebanho_bovino_quad <- dados$Rebanho_bovino^2
dados$PIB_quad <- dados$PIB^2

# Verificar as transformações
print(dados[c("Volume_quad", "Cambio_quad", "Rebanho_bovino_quad", "PIB_quad")])

# Realizar análise de regressão múltipla com variáveis transformadas
modelo_quad <- lm(Desmatamento_ha ~ Volume_quad + Cambio_quad + Rebanho_bovino_quad + PIB_quad, data = dados)
summary(modelo_quad)

# Calcular a matriz de correlação para as variáveis em log
correlacao_quad <- cor(dados[c("Desmatamento_ha", "Volume_quad", "Cambio_quad", "Rebanho_bovino_quad", "PIB_quad")])


# Plotar a matriz de correlação com destaque para as diferenças - teste 2
corrplot(correlacao_quad, method = "color", type = "lower", order = "hclust", 
         tl.cex = 0.5, tl.col = "black", col = colorRampPalette(c("darkgreen", "white", "brown"))(200),
         number.cex = 0.6, addrect = 4)

##PACOTES NECESSÁRIOS
 if (!require("pacman")) install.packages("pacman")
Loading required package: pacman
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

############FLONAS, FLOTAS E GLEBAS
# Calcular o percentual de desmatamento em relação à área total por ano
concessao$percentual_desmatamento <- (concessao$area_desmat / concessao$area_ha) * 100

# Visualizar o DataFrame com a nova coluna adicionada
print(concessao)

# Supondo que seu dataframe já esteja carregado como 'concessao'

# Converter as colunas 'ano_desmat' e 'percentual_desmatamento' para o tipo de dados numérico
concessao$ano_desmat <- as.factor(concessao$ano_desmat)
concessao$percentual_desmatamento <- as.numeric(concessao$percentual_desmatamento)

# Criar o gráfico de linhas
ggplot(concessao, aes(x = ano_desmat, y = percentual_desmatamento, group = nome_uc1)) +
  geom_line(aes(color = factor(nome_uc1))) +
  scale_color_manual(values = palette) +  # Aplicar a paleta de cores personalizada
    labs(title = "",
       x = "Ano",
       y = "Percentual de Desmatamento",
       color = "Área") +
  theme_minimal()


# Calcular o percentual de desmatamento em relação à área total por ano
umf$percentual_desmatamento <- (umf$area_desmat / umf$area_ha) * 100

# Converter as colunas 'ano_desmat' e 'percentual_desmatamento' para o tipo de dados numérico
umf$ano_desmat <- as.factor(umf$ano_desmat)
umf$percentual_desmatamento <- as.numeric(umf$percentual_desmatamento)

Grafico

palette <- c("#8c510a", "#bf812d", "#dfc27d", "#80cdc1", "#35978f", "#01665e", "#003c30",
                      "#ae017e", "#f768a1", "#fcc5c0", "#7a0177", "#c51b8a", "#fc8d59", "#99000d", "#d7301f", "#ef6548", 
                      "#fc8d59", "#fdbb84")
                      
ggplot(umf, aes(x = ano_desmat, y = percentual_desmatamento, group = nome)) +
  geom_line(aes(color = factor(nome))) +
  scale_color_manual(values = palette) +  # Aplicar a paleta de cores personalizada
  labs(title = "",
       x = "Ano",
       y = "Percentual de Desmatamento",
       color = "UMF") +
  theme_minimal()


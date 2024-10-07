##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Criando o gráfico com dois eixos y
ggplot(desmatamento_volume, aes(x = Ano)) +
  geom_line(aes(y = Desmatamento_km, color = "Desmatamento (km²)")) +
  geom_line(aes(y = Volume/1000, color = "Volume Tora (milhões de m³)")) +
  scale_color_manual(values = c("Desmatamento (km²)" = "orange3", "Volume Tora (milhões de m³)" = "brown")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE),   # Desabilitando notação científica
                     sec.axis = sec_axis(~./1000, name = "Volume (milhões de m³)")) +
  labs(y = "Desmatamento (km²)",
       color = "",
       title = "") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(desmatamento_volume$Ano))  # Mostrar todos os anos no eixo x

#OPACAO 2 DO GRAFICO - Criando o gráfico com dois eixos y
ggplot(desmatamento_volume, aes(x = Ano)) +
  geom_line(aes(y = Desmatamento_km, color = "Desmatamento (km²)")) +
  geom_line(aes(y = Volume/1000, color = "Volume Tora (milhões de m³)")) +
  scale_color_manual(values = c("Desmatamento (km²)" = "orange3", "Volume Tora (milhões de m³)" = "brown")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE),   # Desabilitando notação científica
                     sec.axis = sec_axis(~./1000, name = "Volume (milhões de m³)")) +
  labs(y = "Desmatamento (km²)",
       color = "",
       title = "") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(desmatamento_volume$Ano)) +  # Mostrar todos os anos no eixo x
  theme(legend.position = "bottom",  # Posiciona a legenda na parte inferior
        legend.box = "horizontal",  # Exibe a legenda em uma caixa horizontal
        axis.text.x = element_text(angle = 45, hjust = 1))  # Ajusta a angulação dos anos no eixo x para 25 graus e alinha à direita


#Calcular a correlação entre Volume e Desmatamento
correlacao <- cor(desmatamento_volume$Volume, desmatamento_volume$Desmatamento_km)

# Adicionar a reta de projeção para cada ano
reta_projecao <- lm(Volume ~ Desmatamento_km, data = desmatamento_volume)

# Plotar o gráfico de dispersão com facet_grid e linha de projeção
ggplot(desmatamento_volume, aes(x = Desmatamento_km, y = Volume/1000)) +
  geom_point(color = "brown", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "dashed") +
  geom_text(aes(label = paste("r =", round(correlacao, 2))), x = Inf, y = Inf, hjust = 1, vjust = 1, color = "black", size = 4) +
  labs(title = "",
       x = "Desmatamento (km²)",
       y = "Volume (milhões m³)",
       caption = "Fonte: Dados Timberflow e Prodes") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 10, face = "bold"))

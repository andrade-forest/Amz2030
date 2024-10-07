

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

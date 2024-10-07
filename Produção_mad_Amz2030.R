##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
################################
################################ARRUMANDO OS DADOS
################################
df_final <- df_final %>%
  filter(!(uf_origem %in% c("PA", "MT", "SP")))

merged_PA <- merged_PA %>%
  filter(source != "DOF")

merged_MT <- merged_MT %>%
           filter( source != "DOF")
                  
# Juntar os data frames
producao <- bind_rows(merged_MT, merged_PA, df_final)

################################
###############PRODUÇÃO#################
################################

# Calcular a soma do volume por ano
soma_volume_por_ano <- producao %>%
  group_by(ano, uf_origem, produto_group, source) %>%
  summarize(soma_volume = sum(volume, na.rm = TRUE))

# Filtrar as linhas onde produto_group é igual a "tora"
tora <- soma_volume_por_ano %>%
  filter(produto_group == "tora")

# Filtrar as linhas onde produto_group é igual a "tora"
tora_total<- tora %>%
  filter(produto_group == "tora") %>%
  group_by(ano) %>%
  summarize(soma_volume = sum(soma_volume)) %>%
  mutate(uf_origem = "Total")

tora2 <- bind_rows(tora, tora_total)

tora2$ano=as.factor(tora2$ano)

palette <- c("#8c510a", "#bf812d", "#dfc27d", "orange", "brown", "#80cdc1", "#35978f", "#01665e", "#003c30")

ggplot(tora2, aes(x = ano, y = soma_volume, group = uf_origem)) +
  geom_line(aes(color = factor(uf_origem)), size = 0.8) +  # Ajustar a espessura das linhas
  scale_color_manual(values = palette) +  # Aplicar a paleta de cores personalizada
  scale_y_continuous(labels = function(x) paste0(x / 1000000, "M"), breaks = seq(0, max(tora2$soma_volume), by = 1000000), limits = c(0, max(tora2$soma_volume) * 1.1)) +  # Formatar o eixo y em milhões
  labs(title = "", x = "Ano", y = "Volume (m³ Milhões)", color = "Estado") +
  theme_minimal()


################################
###############ESPÉCIES#################
################################

# Calcular a soma do volume por ano
soma_volume_por_ano <- producao %>%
  group_by(ano, nome_cientifico, produto_group) %>%
  summarize(soma_volume = sum(volume, na.rm = TRUE))

# Filtrar as linhas onde produto_group é igual a "tora"
ssp_tora <- soma_volume_por_ano %>%
  filter(produto_group == "tora")

ssp_tora_import <- ssp_tora  %>%
  group_by(nome_cientifico) %>%
  summarize(volume = sum(soma_volume, na.rm = TRUE))


# Ordenar o dataframe pela soma_volume em ordem decrescente
ssp_tora_ordenado <- ssp_tora %>%
  arrange(desc(soma_volume))

# Selecionar as 10 espécies mais importantes em termos de volume
top_10_especies <- head(ssp_tora_ordenado, 10)


##ABRIR TORA2_AJUSTADO PELO EXCEL
ssp_tora_limpo <- ssp_tora2csv  %>%
  group_by(nome_cientifico) %>%
  summarize(volume = sum(volume, na.rm = TRUE))

# Ordenar o dataframe pela soma_volume em ordem decrescente
ssp_tora_limpo <- ssp_tora_limpo %>%
  arrange(desc(volume))

# Selecionar as 10 espécies mais importantes em termos de volume
top_20_especies <- head(ssp_tora_limpo, 20)

# Criar o vetor com os objetivos
especies <- c("Manilkara huberi", "Erisma uncinatum", "Goupia glabra", "Dinizia excelsa", "Ruizterania albiflora", 
              "Qualea paraensis", "Couratari guianensis", "Dipteryx odorata*", "Hymenaea courbaril", 
              "Hymenolobium petraeum", "Mezilaurus itauba**", "Handroanthus serratifolius*", "Qualea ssp", 
              "Apuleia leiocarpa**", "Astronium lecointei", "Cariniana micrantha", "Apuleia molaris", 
              "Schizolobium parahyba", "Trattinnickia burseraefolia", "Cedrelinga cateniformis")

# Adicionar a nova coluna "especie" ao dataframe com os objetivos
top_20_especies$especie <- especies


# Substitua "dados" pelo nome do seu dataframe
ggplot(top_20_especies, aes(x = volume, y = reorder(especies, volume))) +  
  geom_bar(stat = "identity", fill = "#CD853F", alpha = 0.9) +  # Adicionando transparência com alpha = 0.7
  labs(title = "",
       x = "Volume em tora acumulado (m³) 2010-2023",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, hjust = 0, face = "italic"),  # Colocando os nomes em itálico
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  scale_x_continuous(labels = function(x) {
    ifelse(x >= 1000000, paste0(x / 1000000, " M"), paste0(x / 1000, " "))
  }) +
  annotate("text", x = Inf, y = 1.80, hjust = 1, vjust = 0, label = "*Espécie presente no Anexo II da CITES", size = 3) +
  annotate("text", x = Inf, y = 1.25, hjust = 1, vjust = 0, label = "**Espécie Vulnerável - Portaria MMA 148/22", size = 3)


################################
###############POLOS PRODUTORES#################
################################

# Filtrar as linhas onde produto_group é igual a "tora"
tora <- producao %>%
  filter(produto_group == "tora")

municipio_tora <- tora %>%
  group_by(uf_origem, nm_mun_origem, ano) %>%
  summarize(soma_volume = sum(volume, na.rm = TRUE))

# Ordenar o dataframe pela soma_volume em ordem decrescente
munici <- municipio_tora %>%
  arrange(desc(soma_volume))

# Selecionar as 10 espécies mais importantes em termos de volume
mun_15 <- head(munici, 15)

media_volume_por_ano_mun <- municipio_tora %>%
  group_by(nm_mun_origem) %>%
  summarize(media_volume = mean(soma_volume),
            n = n(), # número de observações
            soma_volume = sum(soma_volume, na.rm = TRUE))

# Calcular a média da coluna media_volume
media_total <- media_volume_por_ano_mun %>%
  summarize(media_total_volume = mean(media_volume))

# Visualizar a média total
media_total

################################
###############POLOS CONSUMIDORES#################
################################

# Filtrar as linhas onde produto_group é igual a "tora"
tora <- producao %>%
  filter(produto_group == "tora")

municipio_tora <- tora %>%
  group_by(uf_destino, nm_mun_destino) %>%
  summarize(soma_volume = sum(volume, na.rm = TRUE))

# Ordenar o dataframe pela soma_volume em ordem decrescente
munici <- municipio_tora %>%
  arrange(desc(soma_volume))

# Selecionar as 10 espécies mais importantes em termos de volume
mun_15 <- head(munici, 15)

media_volume_por_ano_mun <- municipio_tora %>%
  group_by(nm_mun_destino) %>%
  summarize(media_volume = mean(soma_volume),
            n = n(), # número de observações
            soma_volume = sum(soma_volume, na.rm = TRUE))

# Calcular a média da coluna media_volume
media_total <- munici %>%
  summarize(soma_volume = sum(soma_volume))

# Visualizar a média total


################################
###############DESTINOS##########
################################

# Transformar todas as letras em minúsculas na coluna 'nome'
  #producao$produto_group <- tolower(producao$produto_group)

# Remover acentos na coluna 'nome'
  #producao$produto_group <- iconv(producao$produto_group, "UTF-8", "ASCII//TRANSLIT")

# Filtrar as linhas onde produto_group é igual a "tora"
  #municipio_tora <- producao %>%
#group_by(uf_origem, nm_mun_origem,uf_destino, nm_mun_destino, ano, produto_group) %>%
#summarize(soma_volume = sum(volume, na.rm = TRUE))

# Filtrar as linhas onde produto_group é igual a "tora"
  #municipio_tora_semano <- producao %>%
#group_by(uf_origem, nm_mun_origem,uf_destino, nm_mun_destino, produto_group) %>%
  #summarize(soma_volume = sum(volume, na.rm = TRUE))

# Filtrar as linhas onde produto_group é igual a "tora"
  #uf_semano <- producao %>%
#group_by(uf_origem,uf_destino, produto_group) %>%
#summarize(soma_volume = sum(volume, na.rm = TRUE))

# Filtrar as linhas onde produto_group é igual a "tora"
  #uf_produto <- uf_semano %>%
#group_by(uf_destino, produto_group) %>%
#summarize(soma_volume = sum(soma_volume, na.rm = TRUE))

# Filtrar as linhas onde produto_group é igual a "tora"
uf_produto_tora <- uf_semano %>%
  filter(produto_group == "tora") %>%
  group_by(uf_destino, produto_group) %>%
  summarize(soma_volume = sum(soma_volume, na.rm = TRUE))

# Filtrar as linhas onde produto_group é igual a um dos três produtos desejados
uf_produto_filtrado2 <- uf_semano %>%
  filter(produto_group %in% c("residuos industriais")) %>%
  group_by(uf_destino, produto_group) %>%
  summarize(soma_volume = sum(soma_volume, na.rm = TRUE))


# Excluir linhas onde a coluna "uf_destino" seja NA
uf_produto_filtrado2 <- uf_produto_filtrado2 %>%
  filter(!is.na(uf_destino))


# Filtrar as linhas onde produto_group é igual a "tora"
destino_produto <- uf_produto_filtrado2 %>%
  group_by(uf_destino) %>%
  summarize(soma_volume = sum(soma_volume, na.rm = TRUE))

sum(destino_produto$soma_volume)
sum(uf_produto_filtrado2$soma_volume)

# Classificar os municípios com base em soma_volume
destino_produto2 <- destino_produto %>%
  mutate(rank = rank(-soma_volume)) %>%
  mutate(destino_classificado = if_else(rank <= 5, as.character(uf_destino), "outros")) %>%
  select(-rank)

destino_produto2 <- destino_produto2 %>%
  group_by(destino_classificado) %>%
  summarize(soma_volume = sum(soma_volume, na.rm = TRUE))


# Definir paleta de cores de marrom
cores_marrom <- c("#3B1415", "#A0522D", "#CD853F", "orange", "#8B4513", "brown", "#CD856F", "#D2691E", "#8B4513", "brown", "#CD853F")

# Criar o gráfico de pizza
grafico_pizza <- ggplot(destino_produto2, aes(x = "", y = soma_volume, fill = destino_classificado)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = cores_marrom) +
  theme(legend.position = "bottom")

# Exibir o gráfico
print(grafico_pizza)

# Reordenar os níveis de destino_classificado com base na soma_volume
destino_produto2$destino_classificado <- factor(destino_produto2$destino_classificado, 
                                                levels = destino_produto2$destino_classificado[order(-destino_produto2$soma_volume)])

# Criar o gráfico de pizza
grafico_pizza <- ggplot(destino_produto2, aes(x = "", y = soma_volume, fill = destino_classificado)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = cores_marrom) +
  theme(legend.position = "bottom") +
  labs(fill = "Destino")

# Exibir o gráfico
print(grafico_pizza)

###criar tabela
produtos <- uf_semano %>%
  group_by(produto_group) %>%
  summarize(soma_volume = sum(soma_volume, na.rm = TRUE))

################################
###############PRODUTOS##########
################################

###criar tabela
produtos2 <- producao %>%
  group_by(produto_group, produto_original) %>%
  summarize(soma_volume = sum(volume, na.rm = TRUE))

# Paleta de cores mais escuras de marrom ao amarelo
cores <- c("#8B4513", "#CD853F","#543C16", "#7E5E2B", "brown", "#D9C874", "#E7DFA0", "#F6EDCC")


# Calcular os percentuais de cada produto
produtos2 <- produtos2 %>%
  mutate(Percentual = (Volume / sum(Volume)) * 100)

# Filtrar os produtos que representam menos de 2%
produtos_filtrados <- produtos2 %>%
  filter(Percentual > 2)

# Calcular o percentual total dos produtos filtrados
percentual_outros <- sum(produtos_filtrados$Percentual)

# Criar uma nova coluna Produto_Ajustado, agrupando os produtos filtrados e outros como "Outros"
produtos2 <- produtos2 %>%
  mutate(Produto_Ajustado = ifelse(Percentual > 2, Produto, "outros"))

# Agrupar os dados pela coluna Produto_Ajustado e somar os volumes para obter os volumes ajustados
produtos_agrupados <- produtos2 %>%
  group_by(Produto_Ajustado) %>%
  summarise(Volume = sum(Volume))

# Plotar o gráfico de pizza com os dados ajustados e ordenados pelo volume
ggplot(produtos_agrupados, aes(x = "", y = Volume, fill = reorder(Produto_Ajustado, -Volume))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Volume / sum(produtos_agrupados$Volume) * 100, ), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  labs(x = NULL, y = NULL, title = "") +
  theme_void() +
  scale_fill_manual(values = c(cores, "grey")) +
  labs(fill = "Produtos de Madeira")


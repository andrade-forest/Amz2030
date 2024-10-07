# Carregue o pacote dplyr
library(dplyr)

##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)


#Lista original de CNPJs
cnpjs <- c("07.909.776/0003-30", "07.909.776/0005-00", "07.909.776/0001-78",
           "07.909.776/0003-30", "04.513.417/0001-09", "04.513.417/0003-70",
           "03.278.503/0002-00", "03.278.503/0001-11", "03.278.503/0001-11",
           "08.759.125/0001-01", "08.759.125/0002-92", "06.036.051/0002-30",
           "06.036.051/0001-50", "06.036.051/0001-50", "15.294.432/0001-20",
           "15.294.432/0003-91", "15.294.432/0004-72", "15.294.432/0005-53",
           "09.263.182/0002-49", "09.263.182/0002-49", "09.263.182/0001-68",
           "09.263.182/0001-68", "02.316.468/0001-15", "02.316.468/0002-04",
           "02.316.468/0005-49", "10.372.884/0001-69", "10.372.884/0001-69",
           "10.372.884/0004-01", "10.372.884/0002-40", "10.372.884/0003-20",
           "04.393.943/0001-82", "04.393.943/0001-82", "13.148.025/0001-89",
           "13.148.025/0001-89", "03.431.797/0002-50", "03.431.797/0001-70",
           "03.431.797/0004-12", "03.431.797/0001-70", "03.431.797/0002-50",
           "03.431.797/0004-12", "03.431.797/0006-84", "03.431.797/0007-65",
           "03.431.797/0008-46", "04.348.929/0006-71", "04.348.929/0006-71",
           "04.348.929/0002-48", "04.348.929/0002-48", "04.348.929/0006-71",
           "29.325.091/0002-06", "29.325.091/0002-06", "04.499.792/0001-41",
           "04.499.792/0001-41", "04.499.792/0001-41", "04.499.792/0001-41",
           "04.499.792/0001-41", "04.499.792/0001-41", "04.499.792/0001-41",
           "05.334.363/0002-68", "05.334.363/0002-68", "05.334.363/0002-68",
           "21.958.623/0001-4")

cnpjs_limpas <- gsub("[-./]", "", cnpjs)


# verificar se algum dos CNPJs está presente na coluna cpf_cnpj_remetente
cnpjs_presentes <- producao %>%
  filter(cpf_cnpj_remetente %in% cnpjs_limpas)

#Lista de ssp
ssp_filtradas <- c("manilkara huberi", "handroanthus ssp", "dipteryx odorata", "mezilaurus itauba", "handroanthus serratifolius")

# verificar estas especies
ssp <- cnpjs_presentes %>%
  filter(nome_cientifico %in% ssp_filtradas)

# pra > de 2015
ssp_2015 <- ssp %>%
  filter(ano > 2014)

##todos

# ssp tod
ssp_selecionadas <- producao %>%
  filter(nome_cientifico %in% ssp_filtradas)

# teste> de 20157
ssp_test <-ssp_selecionadas %>%
  filter(ano > 2019)


# Filtrar as cinco primeiras linhas para cada uf_origem
df_filtrado <- ssp_test %>%
  group_by(uf_origem) %>%
  slice_head(n = 20) %>%
  ungroup()

write.csv(df_filtrado, "teste_timberflow.csv", row.names = FALSE)



#Libraries
library(testthat)
library(rlang)
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(grid)
library(tibble)
library(ggpubr)
library(ggiraph)
library(gganimate)
library(plotly)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(htmltools)

#for maps
library(maps)
library(leaflet)
library(geojsonio)
library(sf)

#para média rolante
library(zoo)

# Set working directory
setwd("~/Desktop/Treino Estágio 2020-2021")

#Data
covid19pt <-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)

covid19pt_testes <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/amostras.csv")

#Transformar para formato de data
covid19pt$data <- as.Date(covid19pt$data,"%d-%m-%Y")
covid19pt_testes$data <- as.Date(covid19pt_testes$data,"%d-%m-%Y")

#Mapa de Portugal
mapa_pt <- geojson_read("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/extra/mapas/portugal.geojson",what = "sp")




# SINTOMAS
##Frequência relativa de sintomas por casos confirmados (últimos dados recolhidos)
## fazer uma tabela e fazer um gráfico de barras

sintomas_geral <- as.data.frame(t(covid19pt[173,41:46])) %>%
  rownames_to_column(var="Sintomas")
  names(sintomas_geral)[2] <- "Frequência"

ggplot(data=sintomas_geral, aes(x = Sintomas, y = Frequência*100)) +
  geom_col(colour= "black", fill="#DD8888", width = 0.6) +
  scale_x_discrete(labels= c("Cefaleia", "Dificuldade\nrespiratória", "Dores musculares", "Febre", "Fraqueza generalizada", "Tosse")) +
  theme_classic() +
  labs(y="Frequência (%)", title = "Frequência de Sintomas da COVID-19",x="Sintomas") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Frequência, digits=4)), vjust=-0.5)

  
## Avaliar se as frequências relativas se foram alterando ao longo do tempo
### Equação para saber a % sintomas diário ( = nº casos tosse do dia / nº casos confirmados diários/novos)
####((sintomas do dia x nº confirmados até ao dia) - (sintomas dia anterior*confirmados dia anterior)) / confirmados novos
sintomas_tempo <- as.data.frame((covid19pt[8:173, 41:46]*covid19pt$confirmados[8:173] - covid19pt[7:172, 41:46]*covid19pt$confirmados[7:172]) / covid19pt$confirmados_novos[8:173])

### Adicionar a linha 7 - 1ª linha (valor de 6 era NA e não dava para ser subtraído antes) 
sintomas_tempo2 <- rbind(covid19pt[7,41:46], sintomas_tempo)

### Combinar com as datas
sintomas_tempo3 <- cbind(covid19pt$data[7:173], sintomas_tempo2)

sintomas_tempo3[3,4] <- 0.11  #1ª linha da dificuldade respiratória à qual tinha sido subtraído NA.

names(sintomas_tempo3) <- c("Data", "Tosse", "Febre", "Dificuldades Respiratórias", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")

sintomas_tempo_melt <- melt(sintomas_tempo3, id.vars="Data")
names(sintomas_tempo_melt)[2:3] <- c("Sintomas", "Valores")

## Fazer um gráfico de linhas
ggplot(sintomas_tempo_melt, aes(x = Data, y = Valores*100, color = Sintomas, group=1)) +
  geom_line() +
  facet_grid(sintomas_tempo_melt$Sintomas) +
  guides(color = FALSE) +
  scale_color_discrete(labels = c("Tosse", "Febre", "Dificuldade Respiratória", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")) +
  xlab("Sintomas") +
  ylab("Frequência (%)") +
  labs(title="Evolução de sintomas ao longo do tempo") +
  theme(axis.title.y = element_text(margin = margin(r = 15, l = 10),
                                    size = 12),
        strip.text.y = element_text(size = 6, 
                                    angle = 0))


# TESTAGEM
## Evolução do nº de testes realizados
### Tabela com coluna para data e outra para número de testes feitos nesse dia
testes_diarios <- covid19pt_testes[,c(1, 3)]
names(testes_diarios) = c("Data", "Testes")

### Gráfico de pontos e linhas com data no eixo do x e número de testes no eixo do y
testes_diarios_grafico <- ggplot(testes_diarios, aes(x = Data, y = Testes)) + 
  geom_point(color = "cadetblue") +
  geom_line(size = 0.4, color = "cadetblue") +
  labs(x = "", title = "Evolução do nº de Testes") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")

### Fazer com que gráfico seja interativo
ggplotly(testes_diarios_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Testes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))


## Evolução da taxa de testes positivos (nº casos / nº testes * 100)
### Tabela com coluna para data e coluna para número de casos confirmados nesse dia (confirmados_novos)
casos_diarios <- covid19pt[, c(1, 12)]
names(casos_diarios) = c("Data", "Casos")

### Tabela com coluna para data e outra para casos desse dia a dividir por número de testes desse dia
### (ter em consideração que o "testes_positivos" tem menos rows que o "casos_diarios")
testes_positivos <- cbind(testes_diarios[,1], as.data.frame((casos_diarios[1:nrow(testes_diarios),2]/testes_diarios[,2])*100))
names(testes_positivos) = c("Data", "Percentagem_Positivos")

### Fazer gráfico com data no eixo do x e proporção de testes positivos no eixo do y
testes_positivos_grafico <- ggplot(testes_positivos, aes(x = Data, y = Percentagem_Positivos)) +
  geom_line(color = "lightseagreen") +
  geom_point(color = "lightseagreen") +
  labs(x = "",
       y = "Frequência (%)",
       title = "Evolução da taxa de testes positivos") +
  scale_x_date(breaks = "months", date_labels = "%B")

###Fazer com que gráfico seja interativo
ggplotly(testes_positivos_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Positivos (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))


#GERAL
##Evolução da incidência
###Fazer gráfico de linhas com data no eixo do x e número de casos no eixo do y
casos_diarios_grafico <- ggplot(casos_diarios, aes(x = Data, y = Casos))+
  geom_point(color = "coral3") +
  geom_line(size = 0.4, color = "coral3")+
  labs(x = "",
       title = "Evolução da Incidência") +
  theme(axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Fazer com que gráfico seja interativo
ggplotly(casos_diarios_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))


## Mortalidade

#Fazer tabela com coluna para data e outra para óbitos ocorridos em cada dia
mortes_diarias <- as.data.frame(cbind(covid19pt$data, as.data.frame(covid19pt$obitos - lag(covid19pt$obitos))))
names(mortes_diarias) = c("Data", "Mortes")

#Tabela com coluna para data, outra para casos e outra para mortes
casos_mortes_diarios <- merge(casos_diarios, mortes_diarias, by="Data")

#Fazer melt para poder fazer gráfico de linhas
casos_mortes_diarios_melt <- melt(casos_mortes_diarios, id.vars = "Data")

#Fazer gráfico de linhas com data no eixo do x, número no eixo do y e mortes ou casos em cada linha
ggplot(casos_mortes_diarios_melt, aes(x = Data, y = value, color = variable)) +
  geom_line(size = 1) +
  facet_grid(casos_mortes_diarios_melt$variable, scales = "free_y") +
  guides(color = FALSE) +
  labs(x = "", 
       y = "Número de Pessoas",
       title = "Relação entre mortalidade e incidência",
       color = "") +
  scale_color_discrete(labels = c("Casos", "Mortes")) +
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")


# FAIXA ETÁRIA E GÉNERO
## Nº Casos
### Selecionar as colunas de confirmados feminino/masculino para todas as idades e juntá-las numa tabela 
femininos <- as.data.frame(covid19pt %>% 
                             dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))
masculinos <- as.data.frame(covid19pt %>% 
                              dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

### Selecionar o valor mais recente de cada coluna de modo a ficar com o número de casos até ao momento para cada faixa etária e para cada género
casos_femininos_idade <- as.data.frame(lapply(femininos, last))
casos_masculinos_idade <- as.data.frame(lapply(masculinos, last))

### Somar a tabela dos femininos com a dos masculinos o que vai dar o número de casos até ao momento por idade apenas
casos_total_idade <- as.data.frame(casos_femininos_idade + casos_masculinos_idade)

### Criar tabela com uma colunda para a faixa etária e outra para o número de casos femininos e mudar coluna da faixa etária para os nomes adequados
casos_femininos_idade_invertido <- as.data.frame(t(casos_femininos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_femininos_idade_invertido)[2] <- "Femininos"
casos_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

### Criar tabela com uma coluna para a faixa etária e outra para o número de casos masculinos/femininos e mudar coluna da faixa etária para os nomes adequados
casos_masculinos_idade_invertido <- as.data.frame(t(casos_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_masculinos_idade_invertido)[2] <- "Masculinos"
casos_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

casos_total_idade_invertido <- as.data.frame(t(casos_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_total_idade_invertido)[2] <- "Total"
casos_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

### Juntar as 3 tabelas que criámos
casos_fem_masc <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")

### Fazer melt para poder fazer gráfico 
casos_fem_masc_tot_melt <- melt(casos_fem_masc, id.vars = "Idade")
names(casos_fem_masc_tot_melt) = c("Idade", "Genero", "Casos")

### Fazer gráfico de barras com idade no eixo do x, o número de casos no eixo do y e o género em cada barra
casos_fem_masc_tot_grafico <- ggplot(casos_fem_masc_tot_melt, aes(x = Idade, y = Casos, fill = Genero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(casos_fem_masc_tot_melt$Casos + 1000))) +
  theme_classic() +
  labs(x = "",
       y = "Mortes") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text( size = 12),
        axis.text.x = element_text(size=8, 
                                   color = "black"),
        axis.text.y = element_text(size=10,
                                   color = "black")) +
  guides(fill=guide_legend(title="Género")) +
  scale_fill_manual(values = c("deeppink3", "lightblue", "grey60"))

### Fazer gráfico interativo
ggplotly(casos_fem_masc_tot_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Casos",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


## Mortalidade por faixa etária e género
###Selecionar as colunas de obitos feminino para todas as idades e juntá-las numa tabela e fazer o mesmo para o masculino
femininos_mortes <- as.data.frame(covid19pt %>% 
                                    dplyr::select(starts_with("obitos_") & (ends_with("9_f")| ends_with("plus_f"))))
masculinos_mortes <- as.data.frame(covid19pt %>% 
                                     dplyr::select((starts_with("obitos_") & (ends_with("9_m")| ends_with("plus_m")))))

###Selecionar o valor mais recente de cada coluna de modo a ficar com o número de óbitos até ao momento  para cada faixa etária e para cada género
mortes_femininos_idade <- as.data.frame(lapply(femininos_mortes, last))
mortes_masculinos_idade <- as.data.frame(lapply(masculinos_mortes, last))

###Somar a tabela dos femininos com a dos masculinos o que vai dar o número de óbitos até ao momento por idade apenas
mortes_total_idade <- as.data.frame(mortes_femininos_idade + mortes_masculinos_idade)

###Criar tabela com uma coluna para a faixa etária e outra para o número de óbitos femininos/masc/total e mudar coluna da faixa etária para os nomes adequados
mortes_femininos_idade_invertido <- as.data.frame(t(mortes_femininos_idade))%>% 
  rownames_to_column(var = "Idade")
names(mortes_femininos_idade_invertido)[2] <- "Femininos"
mortes_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

mortes_masculinos_idade_invertido <- as.data.frame(t(mortes_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_masculinos_idade_invertido)[2] <- "Masculinos"
mortes_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

mortes_total_idade_invertido <- as.data.frame(t(mortes_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_total_idade_invertido)[2] <- "Total"
mortes_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Juntar as 3 tabelas que criámos
mortes_fem_masc <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_fem_masc_tot <- merge(mortes_fem_masc, mortes_total_idade_invertido, by = "Idade")

###Fazer melt para poder fazer gráfico 
mortes_fem_masc_tot_melt <- melt(mortes_fem_masc_tot, id.vars = "Idade")
names(mortes_fem_masc_tot_melt) = c("Idade", "Genero", "Mortes")

###Fazer gráfico de barras com idade no eixo do x, o número de óbitos no eixo do y e o género em cada barra
mortes_fem_masc_tot_grafico <- ggplot(mortes_fem_masc_tot_melt, aes(x = Idade, y = Mortes, fill = Genero)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortes_fem_masc_tot_melt$Mortes + 100))) +
  theme_classic() +
  labs(x = "",
       y = "Mortes") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  guides(fill=guide_legend(title="género")) +
  scale_fill_manual(values = c("deeppink3", "lightblue", "grey60"))

ggplotly(mortes_fem_masc_tot_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Mortes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))

## Letalidade por faixa etária e género
###Tabela com número de casos confirmados por faixa etária por género
casos_genero_idade <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")
casos_genero_idade_total <-  merge(casos_genero_idade, casos_total_idade_invertido, by = "Idade")

###Tabela com número de óbitos por faixa etária por género
mortes_genero_idade <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_genero_idade_total <-  merge(mortes_genero_idade, mortes_total_idade_invertido, by = "Idade")

###Criar tabela com uma coluna com as faixas etárioas e outra com a letalidade e dar nomes às colunas
letalidade_genero_idade <- cbind(casos_femininos_idade_invertido[,1], (mortes_genero_idade_total[,2:4]/casos_genero_idade_total[,2:4]))
names(letalidade_genero_idade) <- c("Idade", "Feminino", "Masculino", "Total")

###Fazer melt para poder fazer gráfico
letalidade_genero_idade_melt <- melt(letalidade_genero_idade, id.vars = "Idade")

###Fazer gráfico com idade no eixo do x, letalidade no eixo do y e faixa etária em cada linha
letalidade_genero_idade_grafico <- ggplot(letalidade_genero_idade_melt, aes(x = Idade, y = value*100, color = variable, 
                                                                            tooltip = round(value*100, digits = 2), data_id = value)) +
  geom_point_interactive() +
  labs(x ="",
       y = "Letalidade (%)") +
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 20,), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12)) +
  scale_color_discrete(name= "Genéro")

girafe(code = print(letalidade_genero_idade_grafico),
       options = list(
         opts_zoom(max = 2),
         opts_hover(css = "fill:black;"),
         opts_sizing(rescale = TRUE, width = 0.8)
       ))






#FAIXA ETÁRIA
## Prevalência (último valor)
### Dados da população de portugal em <https://www.pordata.pt/Portugal/Popula%C3%A7%C3%A3o+residente++m%C3%A9dia+anual+total+e+por+grupo+et%C3%A1rio-10>
pt_0_9 = 433332	+ 461299
pt_10_19 = 507646 + 549033
pt_20_29 = 544575 + 547505
pt_30_39 = 571355 + 679093
pt_40_49 = 792670 + 782555
pt_50_59 = 747581 + 734540
pt_60_69 = 672758 + 620543
pt_70_79 = 544016 + 429107
pt_80_plus = 352218 + 316442

### Fazer tabela com população por faixa etária
populacao_idades_pt <- as.data.frame(c(pt_0_9, pt_10_19, pt_20_29, pt_30_39, pt_40_49, pt_50_59, pt_60_69, pt_70_79, pt_80_plus))

### Calcular prevalência
prevalencia_idade <- ((as.data.frame(t(casos_total_idade)))*100 / populacao_idades_pt) %>%  
  rownames_to_column(var = "Faixa_Etaria")
prevalencia_idade[,1] = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
names(prevalencia_idade)[2] = "Prevalencia"

### Fazer gráfico com faixa etária no eixo do x e prevalência no eixo do y
prevalencia_idade_grafico <- ggplot(prevalencia_idade, aes(x = Faixa_Etaria, y = Prevalencia)) +
  geom_col(fill = "coral2", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black"))

### Tornar gráfico interativo
ggplotly(prevalencia_idade_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Prevalência (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))


## Incidência cumulativa por faixa etária
### Repetir valores da população por faixa etária para ficar com mesmo número de linhas da base de dados
populacao_idades_pt_rep <- as.data.frame(t(populacao_idades_pt[rep(seq_len(ncol(populacao_idades_pt)), each = nrow(covid19pt))]))

### Cálculo da prevalência
prevalencia_tempo_idade <- cbind(covid19pt$data, ((femininos + masculinos) / populacao_idades_pt_rep)*100)
names(prevalencia_tempo_idade) = c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

### Fazer melt para fazer o gráfico
prevalencia_tempo_idade_melt <- melt(prevalencia_tempo_idade, id.vars = "Data")
names(prevalencia_tempo_idade_melt) = c("Data", "Faixa_Etaria", "Prevalencia")

### Fazer gráfico de linhas com data no eixo do x, prevalência no eixo do y e faixa etária nas cores das linhas
prevalencia_tempo_idade_grafico <- ggplot(prevalencia_tempo_idade_melt, aes(x = Data, y = Prevalencia, color = Faixa_Etaria)) +
  geom_line() +
  labs(x = "") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 6)) +
  scale_x_date(breaks = "months", date_labels = "%B")

### Tornar o grafico interativo
ggplotly(prevalencia_tempo_idade_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Prevalência (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 1)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


## Evolução da Incidência por Faixa Etária (s/ valores cumulativos)
### Pegando nas tabelas que fizémos por género or faixa etária, como os valores na base de dados são cumulativos, fazemos o valor desse dia menos o valor do dia anterior para obtermos o número de novos casos por dia, por faixa etária por género
femininos_novos <- femininos - lag(femininos)
masculinos_novos <- masculinos - lag(masculinos)

### Criar uma tabela com uma coluna para a data e outras colunas com o número de casos por dia por faixa etária apenas que resultam da soma dos novos casos femininos com os novos casos masculinos
casos_total_tempo <- cbind(covid19pt$data, as.data.frame(femininos_novos + masculinos_novos))

### Como  linha 7 que é a primeira em que há registo dos casos já representa o número de casos nesse dia apenas, adicionámos essa linha à tabela e dar nomes às colunas
casos_total_tempo[7, 2:10] <- femininos[7,] + masculinos[7,]
names(casos_total_tempo)<- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

### Fazer melt para fazer o gráfico
casos_total_tempo_melt <- melt(casos_total_tempo, id.vars = "Data")
names(casos_total_tempo_melt) = c("Data", "Faixa_Etaria", "Casos")

### Fazer o gráfico de linhas com a data no eixo do x, o número de casos no eixo do y e a faixa etária em cada linha
casos_total_tempo_grafico <- ggplot(casos_total_tempo_melt, aes(x = Data, y = Casos, fill = Faixa_Etaria)) +
  geom_area(alpha=0.6 , size=.5) +
  labs(x ="", 
       y = "Casos") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(size = 3, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%B")

### Tornar gráfico interativo
ggplotly(casos_total_tempo_grafico) %>% 
  layout(legend = list(x = 1, y = 0))


## Evolução da Taxa de Incidência por Faixa Etária
###Calcular a população de risco subtraindo à população por faixa etária o número de óbitos e o número de casos até ao momento                
populacao_risco_idade <- populacao_idades_pt_rep - femininos - masculinos

###Calcular a taxa de incidência, retirando a coluna das datas
taxa_incidencia_idade <- (casos_total_tempo[,-1]/populacao_risco_idade)*100

###Voltar a adicionar a coluna da data e fazer melt para fazer o gráfico
taxa_incidencia_idade <- cbind(covid19pt$data, taxa_incidencia_idade)
names(taxa_incidencia_idade)[1] = "Data"
taxa_incidencia_idade_melt <- melt(taxa_incidencia_idade, id.vars = "Data")
names(taxa_incidencia_idade_melt) = c("Data", "Faixa_Etaria", "Taxa_Incidencia")

###Fazer gráfico de linhas com data no eixo do x, taxa de incidência no eixo do y e faixa etária nas cores das linhas
ggplot(taxa_incidencia_idade_melt, aes(x = Data, y = Taxa_Incidencia, color = Faixa_Etaria)) +
  geom_line(size = 0.8)+
  facet_grid(taxa_incidencia_idade_melt$Faixa_Etaria)+
  guides(color = FALSE) +
  labs(x = "",
       y ="Taxa de Incidência (%)") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 20),
                                    size = 12),
        axis.text.y = element_text(size = 8),
        strip.text.y = element_text(size = 8, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%B")


## Letalidade por faixa etária e género
### Tabela com número de casos confirmados por faixa etária por género
casos_genero_idade <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")
casos_genero_idade_total <-  merge(casos_genero_idade, casos_total_idade_invertido, by = "Idade")

### Tabela com número de óbitos por faixa etária por género
mortes_genero_idade <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_genero_idade_total <-  merge(mortes_genero_idade, mortes_total_idade_invertido, by = "Idade")

### Criar tabela com uma coluna com as faixas etárioas e outra com a letalidade e dar nomes às colunas
letalidade_genero_idade <- cbind(casos_femininos_idade_invertido[,1], (mortes_genero_idade_total[,2:4]/casos_genero_idade_total[,2:4]))
names(letalidade_genero_idade) <- c("Idade", "Feminino", "Masculino", "Total")

### Fazer melt para poder fazer gráfico
letalidade_genero_idade_melt <- melt(letalidade_genero_idade, id.vars = "Idade")

### Fazer gráfico com idade no eixo do x, letalidade no eixo do y e faixa etária em cada linha
ggplot(letalidade_genero_idade_melt, aes(x = Idade, y = value*100, color = variable, 
                                                                            tooltip = round(value*100, digits = 2), data_id = value)) +
  geom_point_interactive() +
  labs(x ="",
       y = "Letalidade (%)") +
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 20,), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12)) +
  scale_color_discrete(name= "Genéro")

### Animar o gráfico 
girafe(code = print(letalidade_genero_idade_grafico),
       options = list(
         opts_zoom(max = 2),
         opts_hover(css = "fill:black;"),
         opts_sizing(rescale = TRUE, width = 0.8)
       ))








#GÉNERO
## Evolução da Incidência
###Duas tabelas com casos diários, uma para cada género
femininos_total_novos <- as.data.frame(covid19pt$confirmados_f - lag(covid19pt$confirmados_f))
masculinos_total_novos <- as.data.frame(covid19pt$confirmados_m - lag(covid19pt$confirmados_m))

###Juntar as tabelas e adicionar coluna com data
incidencia_genero <- cbind(covid19pt$data, femininos_total_novos, masculinos_total_novos)
names(incidencia_genero) = c("Data", "Feminino", "Masculino")

###Fazer melt para fazer o gráfico
incidencia_genero_melt <- melt(incidencia_genero, id.vars = "Data")
names(incidencia_genero_melt) = c("Data", "Genero", "Incidencia")

###Fazer gráfico de linhas com data no eixo do x, incidencia no eixo do y e género nas cores das linhas
incidencia_genero_grafico <- ggplot(incidencia_genero_melt, aes(x = Data, y = Incidencia, color = Genero)) +
  geom_line(size = 0.4) +
  labs(x = "") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(incidencia_genero_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))

## Evolução da taxa de incidência
### Variáveis com a população total, feminina e masculina com base no INE
populacao_pt = 10295909
mulheres_pt = 5435932
homens_pt = 4859977

### Cálculo da incidência criando uma tabbela para o total, outra para mulheres e outra para homens
incidencia_total <- as.data.frame(covid19pt$confirmados_novos/ (populacao_pt - covid19pt$confirmados - covid19pt$obitos))
incidencia_homens <- as.data.frame((covid19pt$confirmados_m - lag(covid19pt$confirmados_m)) 
                                   / (homens_pt - covid19pt$confirmados_m)) 
incidencia_mulheres <- as.data.frame((covid19pt$confirmados_f - lag(covid19pt$confirmados_f)) 
                                     / (mulheres_pt - covid19pt$confirmados_f))

### Remover valores negativos devido a erro na base de dados original em que valor cumulativo do número casos homens e mulheres era 0 e não devia
incidencia_homens[174:175,] <- NA
incidencia_mulheres[174:175,] <- NA

### Criar uma tabela com as 3 tabelas anteriores, adicionando uma coluna com a data e mudar os nomes das colunas
incidencia <- data.frame(covid19pt$data, incidencia_total*100, incidencia_mulheres*100, incidencia_homens*100)
names(incidencia) <- c("Data", "Total", "Mulheres", "Homens")

### Fazer melt para poder fazer gráfico de linhas e dar nome à coluna do género
incidencia_melt <- melt(incidencia, id.vars = "Data")
names(incidencia_melt) <- c("Data", "Genero", "Incidencia")

### Fazer gráfico de linhas com data no eixo do x, a incidencia no eixo do y e o género em cada linha
ggplot(incidencia_melt, aes(x = Data, y = Incidencia, color = Genero)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.4) +
  facet_grid(incidencia_melt$Genero) +
  theme(legend.position = "none") +
  labs(x = "", 
       y ="Incidência (%)") +
  theme(axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        strip.text.y = element_text(size = 8, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%B")


## Calcular a TAXA DE MORTALIDADE covid por género
## Tabela e Gráfico de barras

    ## Dados INE - 10 295 909 total população ; 4 859 977 homens ; 5 435 932 mulheres
população_total <- 10295909
população_f <- 5435932
população_m <- 4859977

Mortalidade <- last(covid19pt$obitos) / população_total
Mortalidadef <- last(covid19pt$obitos_f) / população_f
Mortalidadem <- last(covid19pt$obitos_m) / população_m

tm_t <- cbind(Mortalidadem, Mortalidadef, Mortalidade)

tm <- as.data.frame(t(tm_t))
tm <- tm %>% 
  rownames_to_column(var="Género")
  names(tm)[2] <- "Taxa_Mortalidade"

ggplot(tm, aes(x=Género, y=Taxa_Mortalidade*100)) + 
  geom_col(fill="grey", width = 0.5) +
  scale_x_discrete(labels= c("Total", "Mulheres", "Homens")) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade - COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Mortalidade, digits=4)), vjust=-0.5)


## Calcular TAXA DE LETALIDADE covid por género
### Tabela e Gráfico de Barras
Letalidadetotal <- last(covid19pt$obitos / covid19pt$confirmados)
Letalidadef <- last(covid19pt$obitos_f / covid19pt$confirmados_f)
Letalidadem <- last(covid19pt$obitos_m / covid19pt$confirmados_m)           

tl_t <- cbind(Letalidadetotal, Letalidadem, Letalidadef)

tl <- as.data.frame(t(tl_t))
tl <- tl %>%
  rownames_to_column(var= "Género")
  names(tl)[2] <- "Taxa_Letalidade"

ggplot(tl, aes(x=Género, y=Taxa_Letalidade*100)) + 
  geom_col(fill="grey", width = 0.5) +
  scale_x_discrete(labels= c("Mulheres", "Homens", "Total")) +
  theme_classic() +
  labs(y="Taxa de letalidade (%)", title = "Taxa de letalidade - COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5)
          
## Evolução da letalidade por género
###Calcular letalidade toal, para mulheres e para homens, criando uma tabela para cada com uma coluna para a  data e outra para os valores da letalidade para cada dia
letalidade_tempo_total <- cbind(covid19pt$data, as.data.frame((covid19pt$obitos / covid19pt$confirmados)*100))
letalidade_tempo_mulheres <- cbind(covid19pt$data, as.data.frame((covid19pt$obitos_f / covid19pt$confirmados_f)*100))
letalidade_tempo_homens <- cbind(covid19pt$data, as.data.frame((covid19pt$obitos_m / covid19pt$confirmados_m)*100))

###Juntar as 3 tabelas numa são mudando os nomes de cada coluna
letalidade_tempo_total_mulheres <- merge(letalidade_tempo_total, letalidade_tempo_mulheres, by ="covid19pt$data")
letalidade_tempo <- merge(letalidade_tempo_total_mulheres, letalidade_tempo_homens, by="covid19pt$data")
names(letalidade_tempo) <- c("Data", "Total", "Mulheres", "Homens")

###Fazer o melt para poder faze um gráfico de linhas
letalidade_tempo_melt <- melt(letalidade_tempo, id.vars = "Data")
names(letalidade_tempo_melt) <- c("Data", "Genero", "Letalidade")

###Fazer gráfico de linhas com a data no eixo do x, a letalidade no eixo do y e o género em cada linha
ggplot(letalidade_tempo_melt, aes(x = Data, y = Letalidade, color = Genero)) +
  geom_line() +
  labs(x = "",
       y = "Letalidade (%)") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(letalidade_tempo_grafico, add_tracey = "Letalidade") %>% 
  layout(legend = list(x = 1, y = 0))




#FAIXA ETÁRIA
##Calcular a taxa de mortalidade para cada faixa etária e adicionar coluna com faixas etárias
mortalidade_idade <- cbind(mortes_femininos_idade_invertido[,1], ((mortes_femininos_idade_invertido[,2] 
                                                                   + mortes_masculinos_idade_invertido[,2])*100/ populacao_idades_pt))
names(mortalidade_idade) = c("Faixa_Etaria", "Taxa_Mortalidade")

###Fazer o gráfico com faixa etária no eixo do x e taxa de mortalidade no eixo do y
ggplot(mortalidade_idade, aes(x = Faixa_Etaria, y = Taxa_Mortalidade)) +
  geom_col(fill = "salmon1", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"))

ggplotly(mortalidade_idade_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Mortalidade (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))


## Taxa de Letalidade ao longo do tempo
###Tabela com o número de mortes total diários por faixa etária
total_mortes_novos <- femininos_mortes + masculinos_mortes

###Tabela com o número de casos totais diários por faixa etária
total_casos_novos <- femininos + masculinos

###Tabela com uma coluna com a data e outras com cada faixa etária onde tem o valor da letalidade total para cada dia e  dar nomes às colunas
letalidade_tempo_idade <- cbind(covid19pt$data, total_mortes_novos /total_casos_novos)
names(letalidade_tempo_idade) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Fazer melt para fazer o gráfico
letalidade_tempo_idade_melt <- melt(letalidade_tempo_idade, id.vars = "Data")

#Fazer o gráfico
ggplot(letalidade_tempo_idade_melt, aes(x = Data, y = value*100, color = variable)) +
  geom_line(size = 1) +
  labs(x ="",
       y = "Letalidade (%)") +
  theme(axis.title.x = element_text(margin = margin(t = 20 ,b = 20), 
                                    size = 15),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 15),
        strip.text.y = element_text(angle = 0),
        legend.title = element_blank()) +
  scale_x_date(breaks = "months", date_labels = "%B")








# REGIÕES comparar regiões relativamente a:
## Nº de casos 

casos_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[,4:10], last))))

casos_região <- casos_região %>% 
  rownames_to_column(var="Regiões")
  names(casos_região)[2] <- "N_casos"

ggplot(casos_região, aes(x=Regiões, y=N_casos)) + 
  geom_col(fill="plum3", width = 0.5) +
  scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="Nº de casos", title = "Nº de casos de COVID-19 por regiões", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=N_casos), vjust=-0.5)

## Mapa do nº de casos
### Definir intervalos que queremos na legenda
bins =  c(0, 300, 1000, 2000, 5000, 7000, 35000, Inf)

### Colocar as Regiões da tabela pela mesma ordem que a dos poligonos
casos_regioes_ordem <- casos_região[c(4, 5, 6, 2, 7, 1, 3),]

### Definir a palete de cores para o mapa
pal <- colorBin("YlOrRd", domain = casos_regioes_ordem[,2], bins = bins)

### Definir legenda que aparece quando se passa o rato pelo mapa
labels <- sprintf(
  "<strong>%s</strong><br/>%g casos",
  casos_regioes_ordem[,1], casos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

### Fazer o mapa
casos_regioes_mapa <- leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal(casos_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = casos_regioes_ordem$Casos, opacity = 0.7, title = "Nº Casos",
            position = "bottomright")

## Prevalência
### Valores da população de cada Região com base nas CCDRs
acores = 242796
alentejo = 503507
algarve = 450484
centro = 2217285
lisboa = 3631738
madeira = 253945
norte = 3575338

### Criar uma tabela com uma coluna para as Regiões e outra para o número de pessoas nessa Região
populacao_regioes <- as.data.frame(c(norte, centro, lisboa, alentejo, algarve, acores, madeira), 
                                   c("norte", "centro", "lisboa", "alentejo", "algarve", "açores", "madeira"))
colnames(populacao_regioes) <- "População"
populacao_regioes_invertido <- t(populacao_regioes)

prevalencia_regiao <- (as.data.frame(t(as.data.frame((lapply(covid19pt[,4:10], last)))))*100 / populacao_regioes_invertido) %>% 
  rownames_to_column(var="Regiao")
colnames(prevalencia_regiao)[2] <- "Prevalencia"
prevalencia_regiao[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

ggplot(prevalencia_regiao, aes(x=Regiao, y=Prevalencia)) + 
  geom_col(fill="palegreen", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(y="Prevalência (%)", x="") +
  theme(axis.title.y = element_text(size = 12))

### Tornar gráfico interativo
ggplotly(prevalencia_regiao_grafico)%>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Prevalência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))

## Mapa da prevalência
prevalencia_regiao_ordem <- prevalencia_regiao[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
prevalencia_regiao_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_prevalencia_regiao <- colorBin("viridis", domain = prevalencia_regiao_ordem$Prevalencia, bins = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, Inf))

labels_prevalencia_regiao <- paste( 
  "<strong>", prevalencia_regiao_ordem[,1],"</strong><br/>", 
  round(prevalencia_regiao_ordem[,2], digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_pt) %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.3, 
              fillOpacity = 1, 
              color = "black", 
              weight = 1,
              fillColor = ~pal_prevalencia_regiao(prevalencia_regiao_ordem$Prevalencia),
              label = labels_prevalencia_regiao, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                          textsize = "13px", 
                                          direction = "auto")) %>% 
  addLegend("bottomleft", 
            pal = pal_prevalencia_regiao, 
            values = prevalencia_regiao_ordem$Prevalencia , 
            opacity = 0.5, 
            title = "Prevalência de confirmados por ARS") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite"), options = layersControlOptions(collapsed = FALSE))


## Evolução da Incidência
###Fazer tabela com número de casos diários por região
incidencia_regioes_tempo <- cbind(covid19pt$data, (as.data.frame(covid19pt[, 4:10] -lag(covid19pt[,4:10]))))
names(incidencia_regioes_tempo) =  c("Data", "Norte", "Centro", "LVT", "Alentejo", 
                                     "Algarve", "Açores", "Madeira")
incidencia_regioes_tempo_melt <- melt(incidencia_regioes_tempo, id.vars = "Data")
names(incidencia_regioes_tempo_melt) = c("Data", "Regiao", "Incidencia")

###Fazer gráfico de linhas com data no eixo do x, a incidencia no eixo do y e o género em cada linha
incidencia_regioes_tempo_grafico <- ggplot(incidencia_regioes_tempo_melt, aes(x = Data, y = Incidencia, color = Regiao)) +
  geom_line(size = 0.4) +
  labs(x = "") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        legend.position = "none") +
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(incidencia_regioes_tempo_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))


### Mapa incidência
### Fazer tabela com novos casos mais recentes
incidencia_regioes_recente <- as.data.frame(t(as.data.frame(lapply((covid19pt[, 4:10])
         - lag(covid19pt[, 4:10]), last)))) %>% 
  rownames_to_column(var = "Regioes")
incidencia_regioes_recente[,1] = c("Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                   "Algarve", "Açores", "Madeira")

### Definir intervalos para legenda
bins_7 =  c(0, 25, 50, 100, 200, 300, 400, Inf)

### Definir palete de cores para mapa
pal_7 <- colorBin("YlOrRd", domain = incidencia_regioes_recente[,2], bins = bins_7)

##Da tabela anterior com todas as datas, selecionar apenas o valor mais recente e mudar nome coluna e Regiãoes
incidencia_regioes_recente <- incidencia_regioes_recente[c(4, 5, 6, 2, 7, 1, 3),] 

##Definir legenda quando se passa com o rato por cima
labels_7 <- sprintf(
  "<strong>%s</strong><br/>%g casos novos",
  incidencia_regioes_recente[,1], round(incidencia_regioes_recente[,2], digits = 4)
) %>% lapply(htmltools::HTML)

##Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_7(incidencia_regioes_recente[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_7,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_7, values = incidencia_regioes_recente[,2], opacity = 0.7, title = "Incidência Atual",
            position = "bottomright")


#Incidência com média rolante
##Fazer a média rolante
incidencia_regioes_tempo_media_rolante <- cbind(incidencia_regioes_tempo[,1], as.data.frame(rollmean(incidencia_regioes_tempo[,2:8], k = 7, fill = NA)))
names(incidencia_regioes_tempo_media_rolante)[1] = "Data"

incidencia_regioes_tempo_media_rolante_melt <- melt(incidencia_regioes_tempo_media_rolante, id.vars = "Data")
names(incidencia_regioes_tempo_media_rolante_melt) = c("Data", "Regiao", "Incidencia")

##Fazer gráfico
ggplot(incidencia_regioes_tempo_media_rolante_melt,                                                            aes(x = Data, y = Incidencia, color = Regiao)) +
  geom_line() +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7)) +
  labs(title = "Média Rolante",
       x = "",
       y = "Incidência") +
  scale_x_date(breaks = "months", date_labels = "%B")

##Tornar gráfico interativo
incidencia_regioes_tempo_media_rolante_melt_grafico_interativo <- ggplotly(incidencia_regioes_tempo_media_rolante_melt_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


## Evolução da Taxa de Incidência
###Fazer com que cada coluna seja uma Região e repetir cada número as vezes necessárias para ficar com o número igual ao das colunas da base de dados
populacao_regioes_rep <- as.data.frame(t(populacao_regioes[rep(seq_len(ncol(populacao_regioes)), each = nrow(covid19pt))]))

###Calcular a incidência em cada Região fazendo os casos novos por Região a dividir pela população da Região menos os confirmados da Região menos os óbitos da Região e dar nomes a cada coluna
incidencia_regioes <- cbind(covid19pt$data, (as.data.frame(covid19pt[, 4:10] - lag(covid19pt[, 4:10]))) 
                            / (populacao_regioes_rep - as.data.frame(covid19pt[,4:10])))
names(incidencia_regioes) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve",
                               "Açores", "Madeira")

###Fazer melt para fazer o gráfico e dar nomes a cada coluna
incidencia_regioes_melt <- melt(incidencia_regioes, id.vars = "Data")
names(incidencia_regioes_melt) <- c("data", "regiao", "valor")

###Fazer o gráfico de linhas com a data no eixo do x, a incidência no eixo do y e a Região em cada linha
ggplot(incidencia_regioes_melt, aes(x = data, y = valor*100, color = regiao)) +
  geom_line() + 
  labs(x = "", 
       y = "Incidência (%)") +
  facet_grid(incidencia_regioes_melt$regiao) +
  theme(axis.title.x = element_text(margin = margin(t = 15, b = 10), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 15, l = 10), 
                                    size = 12),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(size = 8, angle = 0)) +
  guides(color = FALSE) +
  scale_x_date(breaks = "months", date_labels = "%B")


##Mortalidade (nº de mortos) por região
mortes_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[, 49:55], last))))

mortes_região <- mortes_região %>% 
  rownames_to_column(var="Regiões")
names(mortes_região)[2] <- "N_mortes"

ggplot(mortes_região, aes(x=Regiões, y=N_mortes)) + 
  geom_col(fill="plum3", width = 0.5) +
  scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="Nº de óbitos", title = "Óbitos de COVID-19 por regiões", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=N_mortes), vjust=-0.5)

#mapa
##Definir intervalos da legenda
bins_3 =  c(0, 25, 50, 100, 500, 1000, Inf)

##Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortos_regioes_ordem <- mortes_região[c(4, 5, 6, 2, 7, 1, 3),]

##Definir a palete de cores
pal_3 <- colorBin("YlOrRd", domain = mortos_regioes_ordem[,2], bins = bins_3)

##Definir a legenda quando se passa com o rato por cima
labels_3 <- sprintf(
  "<strong>%s</strong><br/>%g Mortos",
  incidencia_regioes_recente[,1], mortos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

##Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_3(mortos_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_3,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_3, values = mortos_regioes_ordem[,2], opacity = 0.7, title = "Nº Mortos",
            position = "bottomright")


## Taxa de Mortalidade (nºmortos/população)
###Criar tabela com uma coluna para as Regiãoes e outra para a mortalidade mais recente e dar nomes apropriados
mortalidade_regioes <- data.frame(t(as.data.frame(lapply(covid19pt[, 49:55], last)) *100) 
                                  / populacao_regioes) %>% 
  rownames_to_column(var = "Regiao")
names(mortalidade_regioes)[2] <- "Mortalidade"
mortalidade_regioes[, 1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

###Fazer gráfico com Regiões no eixo do x e mortaldiade no eixo do y
ggplot(mortalidade_regioes, aes(x = Regiao, y = Mortalidade)) +
  geom_col(fill = "gray", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x="") +
  theme(axis.text.y = element_text(size=12))

ggplotly(mortalidade_regioes_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Mortalidade (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
###mapa
###Definir intervalos da legenda
bins_4 =  c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.02, Inf)

###Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortalidade_regioes_ordem <- mortalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

###Definir a palete de cores
pal_4 <- colorBin("YlOrRd", domain = mortalidade_regioes_ordem[,2], bins = bins_4)

###Definir a legenda quando se passa com o rato por cima
labels_4 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Mortalidade",
  mortalidade_regioes_ordem[,1], round(mortalidade_regioes_ordem[,2], digits =  3)
) %>% lapply(htmltools::HTML)

###Fazer o map
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_4(mortalidade_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_4,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_4, values = mortalidade_regioes_ordem[,2], opacity = 0.7, title = "Mortalidade",
            position = "bottomright")


## Taxa de Letalidade
###Fazer uma tabela com uma coluna com a Região e outra com a letalidade para cada Região e dar nomes adequados
letalidade_regioes <- data.frame(t(as.data.frame(lapply(covid19pt[, 49:55], last))*100 
                                   / (as.data.frame(lapply(covid19pt[, 4:10], last))))) %>% 
  rownames_to_column(var = "Regiao")
names(letalidade_regioes)[2] <- "Letalidade"
letalidade_regioes[, 1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

#Fazer o gráfico com a Região no eixo do x e a letalidade no eixo do y
ggplot(letalidade_regioes, aes(x = Regiao, y = Letalidade)) +
  geom_col(fill = "gray", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.y = element_text(size=12))
letalidade_regioes_grafico_interativo <- ggplotly(letalidade_regioes_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Letalidade (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
##mapa
###Definir intervalos da legenda
bins_5 =  c(0, 1, 2, 3, 4, 5, 6, 7, Inf)

###Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
letalidade_regioes_ordem <- letalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

###Definir a palete de cores
pal_5 <- colorBin("YlOrRd", domain = letalidade_regioes_ordem[,2], bins = bins_5)

###Definir a legenda quando se passa com o rato por cima
labels_5 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Letalidade",
  letalidade_regioes_ordem[,1], round(letalidade_regioes_ordem[,2], digits =  2)
) %>% lapply(htmltools::HTML)

###Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_5(letalidade_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_5,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_5, values = letalidade_regioes_ordem[,2], opacity = 0.7, title = "Letalidade",
            position = "bottomright")


## Taxa de Letalidade ao longo do tempo
###Criar tabela com uma coluna para data e outras colunas uma para cada região tendo lá os valores da letalidade diária e dar nomes às colunas
letalidade_regioes_tempo <- cbind(covid19pt$data, as.data.frame((covid19pt[,49:55]/covid19pt[,4:10]))*100)
names(letalidade_regioes_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                     "Algarve", "Açores", "Madeira")

###Fazer melt para poder fazer o gráfico
letalidade_regioes_tempo_melt <- melt(letalidade_regioes_tempo, id.vars = "Data")
names(letalidade_regioes_tempo_melt) = c("Data", "Regiao", "Letalidade")

###Fazer o gráfico de linhas com data no eixo do x, letalidade no eixo do y e regiao em cada linha
letalidade_regioes_tempo_grafico <- ggplot(letalidade_regioes_tempo_melt, aes(x = Data, y = Letalidade, 
                                                                              color = Regiao)) +
  geom_line() +
  labs(x = "",
       y ="Letalidade (%)") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8)) +
  scale_x_date(breaks = "months", date_labels = "%B")

###Tornar gráfico interativo
ggplotly(letalidade_regioes_tempo_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Letalidade (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))






#RECUPERADOS (Evolução da taxa de recuperados)
##Criar tabela com coluna para data e outra coluna para a percentagem de recuperados em cada dia e dar nomes às colunas
recuperados <- cbind(covid19pt$data, as.data.frame((covid19pt$recuperados / covid19pt$confirmados)*100))
names(recuperados) <- c("Data", "Recuperados")

##Fazer gráfico de linhas com data no eixo do x e percentagem recuperados no eixo y
recuperados_grafico <- ggplot(recuperados, aes(x = Data, y = Recuperados)) +
  geom_line(color = "salmon1", size = 1) +
  labs(x = "",
       y = "Taxa de Recuperados (%)") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(recuperados_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Recuperados (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))





#INTERNADOS / INTERNADOS UCI (nº absoluto e por nº de casos) e média
## Evolução do nº de internados
##Fazer melt das colunas data, internados e internados UCI para ter número de internados em cada dia
internados <- melt(covid19pt[,c(1, 15, 16)], id.vars = "data")

##Mudar o nome do conteúdo da coluna dos internados para internados ou internados UCI
levels(internados$variable)[levels(internados$variable)=="internados"] <- "Internados"
levels(internados$variable)[levels(internados$variable)=="internados_uci"] <- "Internados UCI"
names(internados) = c("data", "tipo_internamento", "internados")

###Fazer gráfico de linhas com data no eixo do x, número de internados no eixo do y e tipo de internamento nas linhas
internados_grafico <- ggplot(internados, aes(x = data, y =internados, color = tipo_internamento)) +
  geom_line(size = 1) +
  labs(x = "", 
       color = "") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 7)) +
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(internados_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Pessoas",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))

## Evolução da taxa de internados na UCI
###Fazer melt para ter tabela com coluna da data, coluna do tipo de internamento e coluna com percentagem de internados que são os internados a dividir pelos confirmados e dar nomes às colunas
internados_confirmados <- melt((cbind(covid19pt$data, (as.data.frame(lapply(covid19pt[,c(15, 16)], 
                                                                            function(x) {(x/covid19pt[, 3])*100}))))), id.vars = "covid19pt$data")
names(internados_confirmados) <- c("data", "internados", "percentagem")

###Mudar o nome do conteúdo da coluna dos internados para internados ou internados UCI
levels(internados_confirmados$internados)[levels(internados_confirmados$internados)=="confirmados"] <- "Internados"
levels(internados_confirmados$internados)[levels(internados_confirmados$internados)=="confirmados.1"] <- "Internados UCI"

###Fazer gráfico de linhas com data no eixo do x, percentagem internados no eixo do y e tipo de internamento em cada linha
internados_confirmados_grafico <- ggplot(internados_confirmados, aes(x = data, y = percentagem, color = internados)) +
  geom_line(size = 1) +
  labs(x = "", 
       color = "") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 7)) +
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(internados_confirmados_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Internamento (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))




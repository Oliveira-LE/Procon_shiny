## Projeto de análise de dados em R - Dashboard online (Shiny) - Alura #
##Luciano Oliveira 10/02/2025                                          #
########################################################################

#Bibliotecas usadas no projeto 
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Organização dos dados 
setwd<-choose.dir()
dir()
dados <- read.csv('reclamacao.csv',encoding = 'UTF-8')
summary(dados)

# Nomes das colunas
colnames(dados)

# Seleção dos dados usados na análise 
reclamacao <- dados %>% select(-X.1, -V1)

# Verificação dos anos
# table(reclamacao$anocalendario)

# Exclusão de registros inválidos 
unique(reclamacao$regiao)
reclamacao <- reclamacao %>% filter(regiao != 'N/D')

# unique(reclamacao$UF)
# unique(reclamacao$Tipo)

unique(reclamacao$Atendida)
reclamacao$Atendida <- gsub(pattern = 'S|Siim',replacement = 'sim',x = reclamacao$Atendida)
reclamacao$Atendida <- gsub(pattern = 'N|nAoo|nao',replacement = 'não',x = reclamacao$Atendida)
reclamacao          <- reclamacao %>% filter(Atendida != '')

unique(reclamacao$Atendida)

unique(reclamacao$SexoConsumidor)  
reclamacao$SexoConsumidor <- gsub(pattern = 'N|NULL',replacement = 'N/I', x = reclamacao$SexoConsumidor)
reclamacao                <- reclamacao %>%   filter(!SexoConsumidor %in% c(''))

unique(reclamacao$SexoConsumidor)

# Salvar dados limpos 
fwrite(reclamacao,'dados_limpos.csv',row.names = F)
reclamacao <- fread('dados_limpos.csv')

# Gráficos interativos 
#Barra vertical (Reclamação Atendida) 
grafico_atendida <- ggplot(reclamacao) +
  geom_bar(aes(Atendida),fill = c('#F8766D','#00BFC4'),stat = 'count') +
  ylab('Quantidade') + 
  theme_bw() + 
  ggtitle('Quantidade de Reclamações Atendidas')

## interatividade no gráfico
grafico_atendida <- ggplotly(grafico_atendida)
grafico_atendida

# Barra horizontal (Reclamação Atendida por UF) 
grafico_uf <- data.frame(table(reclamacao$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
  ggplot(aes(x = reorder(UF,Qtd),y = Qtd, text=paste(" UF:", UF, "<br>", "Qtd:",Qtd))) + 
  geom_bar(fill = '#F8766D',stat = 'identity') +  
  coord_flip() +
  xlab('UF') + 
  ylab('Quantidade') +
  theme_bw() + 
  ggtitle('Quantidade de Reclamações por UF')

grafico_uf <- ggplotly(grafico_uf,tooltip = "text")
grafico_uf

#Gráfico de linha (data de arquivamento)
data.frame(table(as.Date(reclamacao$DataAbertura))) %>%  
  rename(Data = Var1, Qtd=Freq) %>%  
  ggplot(aes(Data,Qtd)) + 
  geom_line(group = 1)

# gráfico de linhas data ano-mes
# ano_mes <- data.frame(table(format(as.Date(dados$DataArquivamento),'%Y-%m'))) %>% 
#            rename(Data = Var1, Qtd=Freq)
# ano_mes$Data <- as.Date(paste(ano_mes$Data,'01',sep = '-'))

grafico_data <- data.frame(table(as.Date(reclamacao$DataArquivamento))) %>%
  rename(Data = Var1, Qtd=Freq) %>% 
  ggplot(aes(as.Date(Data), Qtd)) +
  geom_line(group = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  ggtitle('Quantidade de Reclamações por Ano-Mês') +
  xlab('Data') + 
  ylab('Quantidade') +
  scale_x_date(date_labels = '%b-%Y',breaks = '6 month')

grafico_data <- ggplotly(grafico_data)
grafico_data

# Gráfico de barra (Reclamações atendidas por ano) ####

grafico_atendida_ano <- data.frame(table(reclamacao$anocalendario,reclamacao$Atendida)) %>%
  rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>% ggplot() + 
  geom_bar(aes(x = Ano,y = Qtd, fill = Atendida), stat = 'identity',position = position_dodge2()) + 
  theme_bw() + 
  ylab('Quantidade') +
  ggtitle('Quantidade de Reclamações Atendidas por Ano')

## interatividade no gráfico 
grafico_atendida_ano <- ggplotly(grafico_atendida_ano)
grafico_atendida_ano
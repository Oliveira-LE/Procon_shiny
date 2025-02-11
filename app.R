#                                                                         #
# This is a Shiny web application. You can run the application by clicking#
# the 'Run App' button above.                                             #
#                                                                         #
# Find out more about building applications with Shiny here:              #
#                                                                         #
#    http://shiny.rstudio.com/                                            #
###########################################################################
## Projeto de análise de dados em R - Dashboard online (Shiny) - Alura    #
##Luciano Oliveira 10/02/2025                                             #
###########################################################################

#Bibliotecas usadas no projeto 
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinyWidgets)

# Dados Analisados  
dados <- fread('dados_limpos.csv',encoding = 'UTF-8')

# Resumo 
media_chamados_ano <- dados %>%
  group_by(anocalendario) %>%
  summarise(qtd_chamados = n() ) %>%
  summarise(medias_chamado_ano = mean(qtd_chamados)) %>%
  as.numeric()

# Front-end
cabecalho <- dashboardHeader(title = "Dashboard PROCON")

barra_lateral <- dashboardSidebar(width = '250px',
                                  sidebarMenu(
                                    menuItem('Dashboard',tabName = 'dashboard',icon = icon('dashboard')),
                                    menuItem('Informações',tabName = 'infos',icon = icon('info-circle'))))

painel_principal <- dashboardBody(tags$head(tags$style(HTML(".info-box, .info-box-icon, .small-box{height: 100px}"))),
                                  tabItems(
                                    tabItem(tabName = 'infos',
                                            h1("Informações"),
                                            infoBox(title = 'Contato',
                                                    icon = icon('envelope-square'),
                                                    subtitle = 'Para mais informações e/ou feedback entre em contato: luc.deoliveira@gmail.com')),
                                    tabItem(tabName = 'dashboard',
                                            fluidRow(
                                              valueBox(subtitle = 'Registros',
                                                       value = nrow(dados),
                                                       color = 'aqua',
                                                       icon = icon("database")),
                                              infoBox(title = '',
                                                      subtitle = "Reclamações por Ano",
                                                      color = 'aqua',
                                                      value = media_chamados_ano,icon = icon("list")),
                                              valueBoxOutput(outputId = "qtdUf")),
                                            fluidRow(
                                              column(width = 12,
                                                     box(title = 'Filtros',
                                                         width = '100%',
                                                         column(width = 12,
                                                                box(width = '100%',
                                                                    awesomeCheckboxGroup(inline = TRUE, 
                                                                                         inputId = 'select_UF',
                                                                                         label = 'Estados:', 
                                                                                         choices = c('TODOS',unique(dados$UF)),
                                                                                         selected = 'TODOS'))),
                                                         column(width = 6,
                                                                box(width = '100%',
                                                                    dateRangeInput(inputId = 'data_abertura',
                                                                                   label = 'Data Abertura:',
                                                                                   format = 'dd-mm-yyyy',
                                                                                   start = min(as.Date(dados$DataAbertura)),
                                                                                   end = max(as.Date(dados$DataAbertura))))),
                                                         column(width = 6,
                                                                box(width = '100%',
                                                                    selectizeInput(inputId = 'assunto',
                                                                                   label = 'Descrição Assunto:',
                                                                                   choices = c('TODOS', unique(dados$DescricaoAssunto)),
                                                                                   multiple = T, options = list(maxItems = 5),
                                                                                   selected = 'TODOS')))))),
                                            fluidRow(
                                              column(width = 12,
                                                     box(width = '100%',
                                                         plotlyOutput(outputId = 'data',width = '100%'),
                                                         verbatimTextOutput(outputId = 'descData')))),
                                            fluidRow(
                                              column(width = 6,
                                                     box(width = '100%',
                                                         plotlyOutput(outputId = 'atendida'))),
                                              column(width = 6,
                                                     box(width = '100%',
                                                         plotlyOutput(outputId = 'atendidaAno')))),
                                            fluidRow(
                                              column(width = 12,
                                                     box(width = '100%',title = 'Reclamações por UF',
                                                         plotlyOutput(outputId = 'uf'),
                                                         textOutput(outputId = 'descUf')))))))

ui <- dashboardPage(header = cabecalho, sidebar = barra_lateral, body = painel_principal)

# Define a UI para a aplicação que desenha o histograma
ui2 <- fluidPage(
  
  # Título da aplicação
  titlePanel("Olá Shiny"),
  
  # Sidebar com slider 
  sidebarLayout(sidebarPanel(checkboxGroupInput(inputId = 'select_UF',
                                                label = 'Estados:', 
                                                choices = c('TODOS',unique(dados$UF)),
                                                selected = 'TODOS'),
                             dateRangeInput(inputId = 'data_abertura',
                                            label = 'Data Abertura:',
                                            format = 'dd-mm-yyyy',
                                            start = min(as.Date(dados$DataAbertura)),
                                            end = max(as.Date(dados$DataAbertura))),
                             selectizeInput(inputId = 'assunto',
                                            label = 'Descrição Assunto:',
                                            choices = c('TODOS', unique(dados$DescricaoAssunto)),
                                            multiple = T, 
                                            options = list(maxItems = 5),
                                            selected = 'TODOS')),
                
                # Mostra o gráfico da distribuição gerada
                mainPanel(plotlyOutput(outputId = 'data'), verbatimTextOutput(outputId = 'descData'),
                          plotlyOutput(outputId = 'uf'), textOutput(outputId = 'descUf'),
                          plotlyOutput(outputId = 'atendidaAno'))))

# Defina a lógica do servidor necessária para desenhar um histograma
server<- function(input, output) {
  dados_selecionados <- reactive({
    print(input$data_abertura)
    if(!'TODOS' %in% input$select_UF){
      dados<- dados %>%
        filter(UF %in% input$select_UF)}
    if(! 'TODOS' %in% input$assunto){
      dados<- dados %>%
        filter(DescricaoAssunto %in% input$assunto)}
    dados<- dados %>% 
      filter(as.Date(DataAbertura) >= input$data_abertura[1] &
               as.Date(DataAbertura) <= input$data_abertura[2])})
  
  #Gráfico Quantidade de Reclamações Atendidas por Ano
  output$atendidaAno <- renderPlotly({ggplotly(data.frame(table(dados_selecionados()$anocalendario,
                                                                dados_selecionados()$Atendida)) %>%
                                                 rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
                                                 ggplot() +
                                                 geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                                                          stat = 'identity',position = position_dodge2()) +
                                                 theme_bw() +
                                                 ylab('Quantidade') +
                                                 ggtitle('Quantidade de Reclamações Atendidas por Ano'))})
  
  #Gráfico Quantidade de Reclamações por Ano-Mês  
  output$data <- renderPlotly({ggplotly(data.frame(table(as.Date(dados_selecionados()$DataAbertura))) %>%
                                          rename(Data = Var1, Qtd=Freq) %>%
                                          ggplot(aes(as.Date(Data), Qtd)) +
                                          geom_line(group = 1) +
                                          theme_bw() + 
                                          theme(axis.text.x = element_text(angle = 45,hjust = 1))+
                                          ggtitle('Quantidade de Reclamações por Ano-Mês') +
                                          xlab('Data') + 
                                          ylab('Quantidade') +   
                                          scale_x_date(date_labels = '%b-%Y',breaks = '6 month'))})
  
  #Gráfico Quantidade de Reclamações por UF  
  output$uf <- renderPlotly({ggplotly(data.frame(table(dados_selecionados()$UF)) %>% 
                                        rename(UF = Var1,Qtd = Freq) %>%
                                        ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                                                   text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
                                        geom_bar(fill = '#F8766D',stat = 'identity') +  
                                        coord_flip() +
                                        xlab('UF') + 
                                        ylab('Quantidade') +
                                        theme_bw() + 
                                        ggtitle('Quantidade de Reclamações por UF'),
                                      tooltip = "text")})
  #Gráfico Quantidade de Reclamações Atendidas
  output$atendida <- renderPlotly({ggplotly(ggplot(dados_selecionados()) +
                                              geom_bar(aes(Atendida),fill = c('#F8766D','#00BFC4'),stat = 'count') +
                                              ylab('Quantidade') + 
                                              theme_bw() + 
                                              ggtitle('Quantidade de Reclamações Atendidas'))})
  
  # Retorno do texto para cada campo em específico 
  output$descData <- renderText({paste("Gráfico com a quantidade de reclamações feitas entre: ",
                                       min(dados_selecionados()$DataAbertura),'-',
                                       max(dados_selecionados()$DataAbertura))})
  
  output$descUf <- renderText({estados <- paste(unique(dados_selecionados()$UF), collapse = ',')
  paste('Gráfico com a quantidade de reclamações feitas pelas UF: ', estados)})
  
  output$qtdUf <- renderValueBox({valueBox(value = length(unique(dados_selecionados()$UF)),
                                           subtitle = "UFs Selecionadas",icon = icon("map-marker"))})}

# Executar a aplicação 
shinyApp(ui = ui, server = server)
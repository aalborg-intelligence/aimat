# Load the shiny package
library(shiny)
library(rio)
# library(readxl)
library(DT)
# source("R/nn_generel.R")
library(aimat)
# source("R/nn_viz.R")

ui <- fluidPage(
  titlePanel("Neuralt netværks app"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Vælg træningsdatafil (typisk xlsx eller csv format)'),
      selectInput("type", "Target-type", choices = c("klassifikation", "regression")),
      varSelectizeInput('target', "Target-variabel (Vælges når data er uploaded)", NULL),
      selectInput('feature', "Feature-variable (Vælges efter target-variabel.)", NULL, multiple = TRUE),
      numericInput("hidden1", "Antal neuroner i første skjulte lag", value = 1, min = 0, max = 10),
      numericInput("hidden2", "Antal neuroner i andet skjulte lag", value = 1, min = 0, max = 10),
      numericInput("w_start", "Start-vægte (alle vægte sættes til denne værdi)", value = NA),
      numericInput("lr", "Learning rate.", value = 0.01, min = 0.0001),
      # numericInput("eps", "Stop-kriterie (når relativ ændring i tabsfunktion er under denne stopper algoritmen).", value = 0.0001, min = 0),
      numericInput("max_it", "Maksimalt antal interationer før algoritmen stopper.", value = 1e3, min = 1, max = 1e6),
      # numericInput("k", "Antal fold ('k' i k-fold krydsvalidering)", value = 5, min = 2, max = 10, step = 1),
      selectInput("activation", "Aktiveringsfunktion", choices = c("Sigmoid", "ReLu", "Tangenshyperbolsk", "Softsign", "Identitet")),
      selectInput("loss", "Tabsfunktion", choices = c("Kvadratisk", "Cross-entropy")),
      actionButton("run", "Træn netværk!"),
      textOutput("run_txt"),
      fileInput('file2', 'Vælg testdatafil (typisk xlsx eller csv format)'),
      # h2("Krydsvalidering:"),
      actionButton("run_test", "Kør test!"),
      textOutput("run_test_txt")
    ),
    mainPanel(
      h2("Model information:"),
      strong("Target-variabel: "),
      textOutput("targetinfo"),
      strong("Features: "),
      textOutput("featureinfo"),
      # h2("Vægte: "),
      # textOutput("weightinfo"),
      # dataTableOutput('weights'),
      # textOutput("modelinfo"),
      plotOutput("loss_plot"),
      visNetwork::visNetworkOutput("network"),
      # h2("Krydsvalidering:"),
      # strong(paste0("Gennemsnitlig andel korrekte ved k-fold krydsvalidering:")),
      # textOutput("cv_info"),
      h2("Testdata:"),
      textOutput("testdatainfo"),
      dataTableOutput('testcontents'),
      h2("Træningsdata:"),
      textOutput("datainfo"),
      dataTableOutput('contents'))
  )
)

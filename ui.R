setwd("C:/Users/ozank/Desktop/outbreak_ozan")

library(shiny)
library(DT)
library(lubridate)
library(ggplot2)



data <- data.frame()
val_data <- data.frame()
ui <- fluidPage(
  fluidPage(
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        actionButton("data_validation", label = "Validate the Data")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    id = "upperTabSet",
                    tabPanel("Data", dataTableOutput('contents')),
                    tabPanel("Validated Data", dataTableOutput('table')),
                    tabPanel("Summary", verbatimTextOutput("summary")),
                    tabPanel("Arima", tabsetPanel(type = "tabs",
                                                  id = "innerTabSet",
                                                    tabPanel("Arima",
                                                             fluidRow(
                                                               column(4,numericInput('NumYearsTrain', 'Number of Years to train', 1,
                                                                                       min = 1, max = 9)),
                                                               column(4,selectInput('win_slide', 'Sliding Window (day)', c( "7",
                                                                                                                  "14",
                                                                                                                  "21"), selectize=FALSE)),
                                                               column(4,selectInput('Cusum_Parameters', 'Cusum Parameters', c( "Aggresive",
                                                                                              "Moderate",
                                                                                              "Routine"), selectize=FALSE))),
                                                             actionButton("run_arima", label = "Run Arima"),
                                                             fluidRow(
                                                               column(6,h2("Cusum Results")),
                                                               column(6,h2("Time Series Summary"))
                                                               ),
                                                             
                                                             fluidRow(
                                                               column(6,verbatimTextOutput("cusum_summary")),
                                                               column(6,verbatimTextOutput("ts_summary")))),
                                                    tabPanel("Arima Data", dataTableOutput('arima_data')),
                                                    tabPanel("Residual Plot", plotOutput('arima_summary')),
                                                    #tabPanel("Cusum Summary", verbatimTextOutput('cusum_summary')),
                                                    tabPanel("Final Plots", 
                                                             dataTableOutput('arima_plot'))
                                                    
                    ))
                    
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
#----------------------------------------------------------------------------
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    data <<- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)


    
  })
  output$contents <- renderDataTable(getData(),
                                  options = list(
                                    searching = FALSE, 
                                    pageLength = 15))
  #output$contents <- renderTable(getData())
  
#-----------------------------------------------------------
  getSummary <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    summary(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                                quote=input$quote))
    
  })
  
  

  output$summary <- renderPrint(
    
    getSummary()
    
  )
  
#-------------------------------------------------------------------------
  
  
  
    observeEvent(input$data_validation, {
    data <<- data[,-4] 
    colnames(data) <<- c("Date","Number","Holiday")
    source('Data_validation.R')
    val_data <<- input.date.check(input.date.check(data))
    #output$table <- renderText({input.sanity.check(val_data)})
    #output$table <- renderDataTable(data[1])
    output$table <- renderDataTable(val_data,
                                    options = list(
                                      pageLength = 15,
                                      searching = FALSE)
    )

  })

  
  
#-------------------------------------------------------------------------
  
  observeEvent(input$run_arima, {
    
    source('03_arima_yardimci_ozan.R')
    results <<- arima_run(val_data,input$NumYearsTrain,input$win_slide,input$Cusum_Parameters)
    
    
    output$arima_summary <- renderPlot(ggtsdisplay(residuals(results[[2]], type="normalized"), main="ARIMA errors"))
    output$cusum_summary <- renderPrint(summary(results[[3]]))
    output$ts_summary <- renderPrint(summary(results[[4]]))
    output$arima_data <- renderDataTable(results[[1]],
                                         options = list(
                                           pageLength = 10,
                                           searching = FALSE)
    )

  })

  
}

shinyApp(ui, server)

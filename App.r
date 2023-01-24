library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library (tidyverse)

ui <- dashboardPage(
  
  dashboardHeader(title = "HMO Predicion Tool"),
  dashboardSidebar(
    #Read the data
    fileInput("upload", label="HMO inout file", accept = c(".csv")),
    #Read the actual (solution) data
    fileInput("upload_Solution", label="HMO solution file", accept = c(".csv")),
    #get a number (how much of the dataframe to show)
    numericInput("n", "Number of Rows", value = 5, min = 1, step = 1),
   
    tags$p("This tool is used to predict if HMO customer is going to be expensive or not", style = "margin: 5px 5px 5px")
  ),
  
  dashboardBody(
    
    fluidRow(
      
      h4('Descriptive Analytics on the data uploaded',  style = "margin: 20px 20px 20px")
    ),

    fluidRow(
      box(title = "Age Distribution",
          solidHeader = T,
          collapsible = T,
          width = 6, 
          plotlyOutput("Age_plot"),
          collapsed = T,
          ),
      box(title = "BMI Distribution", solidHeader = T,
          width = 6, collapsible = T,
          plotlyOutput("BMI_plot"),
          collapsed = T
      )
    ),# row
    
    fluidRow(
      box(title = "Smoker Percentage",
          solidHeader = T,
          collapsible = T,
          width = 6, 
          plotlyOutput("smoker_plot"),
          collapsed = T,
      ),
      box(title = "Active Percentage", solidHeader = T,
          width = 6, collapsible = T,
          plotlyOutput("active_plot"),
          collapsed = T,
      )
    ),# row
    fluidRow(
      box(title = "Snapshot of HMO file data", solidHeader = T,
          width = 12, collapsible = T,
         
          dataTableOutput("headForDF"),
          collapsed = T,
         
          )
    ), # row
    
    fluidRow(
      
      h4('Predictive Model results',  style = "margin: 20px 20px 1px")
    ),
    
    fluidRow(
      div( "The box below shows the confusion matrix of our predictive model, we are basically interested to look at three main indicators;Accuracy, Sensetivity and Specificity. The Accuracy is basically how well the model is in general in predicting the true values. Sensetivity on the other hand is how well the model is in predicitng the true positive class (non expensive patients) while Specificity is how well the model is in prediciting the negative class (expensive patients). Based on our model we expect 88% accuracy, 97% sensitivity (97% of the patients who are non expensive will be predicted as non expensive) and 61% Specificity (61% of the patients who are expensive will be predicted as expensive).", style = "margin: 25px 25px 2px"),
      br(),
      div("Sensitivity has the highest importance over other indicatores.",  style = "margin: 2px 25px 25px")
    ),
    
    fluidRow(
      valueBoxOutput("Accuracy"),
      valueBoxOutput("Sensitivity"),
      valueBoxOutput("Specificity")
    ),
    
    fluidRow(
      box(title = "Confusion Matrix and Statistics", solidHeader=T,
          width = 12, collapsible = T,
          #output the results (for now, just simple text)
          verbatimTextOutput("txt_results", placeholder = TRUE)
          ),
    ), # row
    
  
    
    
  ) # body 
  
)


server <- function(input, output) {
  
  #require an input file, then read a CSV file
  getTestData <- reactive({
    req(input$upload)
    
     inFile <- input$upload
     if (is.null(inFile))
       return(NULL)
     read_csv(inFile$datapath)
      })
  
  #require an the actual values for the prediction (i.e. solution file)
  getSolutionData <- reactive({
    req(input$upload_Solution)
    inFile <-input$upload_Solution
    if (is.null(inFile))
      return(NULL)
    read_csv(inFile$datapath)
  })
  
  #show the output of the model
  output$txt_results <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    #Only columns we are interested in
    dataset <- dataset[, c('bmi', 'age','smoker', 'exercise')]
    #load and use the model on the new data
    use_model_to_predict(dataset, dataset_solution)
  })
  
  #show a few lines of the dataframe
  output$headForDF<- renderDataTable({df <- getTestData()
  head(df, input$n) }, 
                  options = list(scrollX = TRUE))
  
  #these libraries are needed, will be used with predict
  library(caret); library(kernlab); library(e1071)
  
  
  #load a model, do prediction and compute the confusion matrix
  use_model_to_predict <- function(df, df_solution){
    #load the pre-built model, we named it ‘out_model.rda’)
 
    #print(path)
    load(file= "cartTree_model.rda")
    
    #use the model with new data
    cartTreePred <- predict(our_model, df,  type = "class")
    
 
    #show how the model performed
    df_solution$expensive <- as.factor(df_solution$expensive)
    
    
    
    confMatrix <- table(cartTreePred, df_solution$expensive)
    errorRate <- (sum(confMatrix)- sum(diag(confMatrix)))/ sum(confMatrix)
    
    accu <- (1-errorRate)*100 
    output$Accuracy <- renderValueBox({
      valueBox(paste0("Accuracy:",  accu), 
               "%", icon = icon("fire"), color = "yellow")
    })
    
    sens <- 100*confMatrix[1,"FALSE"] / (confMatrix[1,"FALSE"] + confMatrix[2,"FALSE"] )
    
    output$Sensitivity <- renderValueBox({
      valueBox(paste0("Sensitivity:",  sens), 
               "%", icon = icon("exclamation-triangle"), color = "green")
    })
    
    spece <- 100*confMatrix[1,"TRUE"] / (confMatrix[1,"TRUE"] + confMatrix[2,"TRUE"] )
    
    output$Specificity <- renderValueBox({
      valueBox(paste0("Specificity:",  spece), 
               "%", icon = icon("fire"), color = "red")
    })
    
    
    confusionMatrix(cartTreePred, df_solution$expensive) 
    
  }
  
  output$BMI_plot <- renderPlotly({
  
    plot_bmi <- ggplot(getTestData(), aes(x=bmi)) + geom_histogram()
    
    ggplotly(plot_bmi)
  })
  
  output$Age_plot <- renderPlotly({
    
    plot_bmi <- ggplot(getTestData(), aes(x=age)) + geom_histogram()
    
    ggplotly(plot_bmi)
  })
  
  output$smoker_plot <- renderPlotly({
   
    plot_smoker <- ggplot(getTestData(), aes(x=reorder(smoker, smoker, function(x)-length(x)))) +
      geom_bar(fill='grey') +  labs(x='Smoker')
    
    ggplotly(plot_smoker)
  })
  
  output$active_plot <- renderPlotly({
   
    plot_active <- ggplot(getTestData(), aes(x=reorder(exercise, exercise, function(x)-length(x)))) +
      geom_bar(fill='grey') +  labs(x='Active')
    
    ggplotly(plot_active)
  })
  
  

}
  
  
  # Run the application 
  shinyApp(ui = ui, server = server)  
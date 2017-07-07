library(shiny)
library(DT)

shinyUI(navbarPage("Proph Gen",
  # Model evaluation --------------------------------------------------------
  tabPanel("Model training",
    sidebarLayout(
      sidebarPanel(
        h3("Prophecy Generator"),
        p("This Shiny app implements Facebook's prophet package for time series forecasting."),
        br(),
        fileInput('df', 'Choose .csv file',
                  accept=c('.csv', 'text/csv', 'text/comma-separated-values,text/plain')),
        
        checkboxInput("log_trans", label = "Log transform y-variable?", value = FALSE),
        
        actionButton("run_model", "Forecast")
      
      ),  # End sidebarPanel
      
      mainPanel(
        plotOutput("forecast_plot"), 
        plotOutput("component_plot")
      )  # End mainPanel
    )  # End sidebarLayout        
       
  ),  # End tabPanel ("Model training")
  
  # Model comparison --------------------------------------------------------  
  tabPanel("Model comparison")
))


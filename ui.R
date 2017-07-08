library(shinydashboard)

ui <- dashboardPage(
  ##################################################################################
  dashboardHeader(title = "Prophecy Generator"),
  
  ##################################################################################
  dashboardSidebar(
    sidebarMenu(
      fileInput('df', 'Choose .csv file',
                accept=c('.csv', 'text/csv', 'text/comma-separated-values,text/plain')),

      checkboxInput("log_trans", label = "Log transform y-variable", value = FALSE),
      
      radioButtons("growth", label = strong("Growth type:"),
                   choices = list("linear" = "linear", "logistic" = "logistic"), 
                   selected = "linear"),
      
      # Display this only if logistic growth is selected
      conditionalPanel(condition = "input.growth == 'logistic'",
        numericInput("cap", label = strong("Add constant cap"), value = 1)
      )
      
    )
  ),
  
  ##################################################################################
  dashboardBody(
    fluidRow(
      actionButton("run_model", "Forecast"),
      br(), br()
    ),
    
    fluidRow(
      tabBox(id = "tabset1", width = 12, height = 500,
        tabPanel("Main", 
          plotOutput("forecast_plot")
        ),
            
        tabPanel("Trend", 
          plotOutput("trend_plot")
        ),
            
        tabPanel("Yearly", 
          plotOutput("yearly_plot")
        ),
            
        tabPanel("Weekly", 
          plotOutput("weekly_plot")
        ),
          
        tabPanel("Holidays", 
          h2("placeholder")
        )
      )  # End tabBox
    )  # End fluidRow
  )  # End dashboardBody
)


library(shinydashboard)
library(DT)

ui <- dashboardPage(
  ##################################################################################
  dashboardHeader(title = "Prophecy Generator"),
  
  ##################################################################################
  dashboardSidebar(
    sidebarMenu(
      fileInput('df', 'Choose .csv file',
                accept=c('.csv', 'text/csv', 'text/comma-separated-values,text/plain')),
      
      checkboxInput("log_trans", label = "Log transform y-variable", value = FALSE),
      
      actionButton("run_model", "Forecast")
    )
  ),
  
  ##################################################################################
  dashboardBody(
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


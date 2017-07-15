library(shinydashboard)

ui <- dashboardPage(
  ##################################################################################
  dashboardHeader(title = "Prophecy Generator"),
  
  ##################################################################################
  dashboardSidebar(
    fileInput('df', 'Upload time series file (.csv):',
              accept=c('.csv', 'text/csv', 'text/comma-separated-values,text/plain')),
      
    fileInput('holiday_df', 'Upload holidays file (.csv):',
              accept=c('.csv', 'text/csv', 'text/comma-separated-values,text/plain')),
  
    checkboxInput("log_trans", label = "Log transform y-variable", value = FALSE),
    
    radioButtons("growth", label = strong("Growth type:"),
                 choices = list("linear" = "linear", "logistic" = "logistic"), 
                 selected = "linear"),
      
    # Display this only if logistic growth is selected
    conditionalPanel(condition = "input.growth == 'logistic'",
      numericInput("cap", label = strong("Add constant cap"), value = 1)
    ),
    
    numericInput("changept_prior_scale", label = strong("Adjust changepoint prior scale:"), value = 0.05),
    
    radioButtons("holiday_effect", label = strong("Add holiday effect"),
                 choices = list("Yes" = "yes", "No" = "no"),
                 selected = "no")
    
    
  ),  # End dashboardSidebar
  
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
          plotOutput("holiday_plot")
        )
      )  # End tabBox
    )  # End fluidRow
  )  # End dashboardBody
)


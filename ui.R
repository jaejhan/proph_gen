library(shinydashboard)
library(DT)

ui <- dashboardPage(
  ##################################################################################
  dashboardHeader(title = "Prophecy Generator"),
  
  ##################################################################################
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model training", tabName = "model_train", icon = icon("dashboard")),
      menuItem("Model comparison", tabName = "model_eval", icon = icon("th")),
      
      br(),
      
      fileInput('df', 'Choose .csv file',
                accept=c('.csv', 'text/csv', 'text/comma-separated-values,text/plain')),
      
      checkboxInput("log_trans", label = "Log transform y-variable?", value = FALSE),
      
      actionButton("run_model", "Forecast")
    )
  ),
  
  ##################################################################################
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "model_train",
        fluidRow(
          box(
            plotOutput("forecast_plot")
          ),
                
          box(
            plotOutput("component_plot")
          )
        )
      ),
      
      # Second tab content
      tabItem(tabName = "model_eval",
        h2("Widgets tab content")
      )
    )
  )  # End dashboardBody

)


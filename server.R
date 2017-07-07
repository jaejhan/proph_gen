library(shinydashboard)
library(prophet)
library(readr)
library(DT)
load("data/proph_gen_ex.Rdata")

server <- function(input, output) {
  # Prepare dataset -----------------------------------------------------------
  df <- reactive({
    if (is.null(input$df)) {
      proph_gen_ex
    } else {
      inFile <- input$df
      df <- read_csv(inFile$datapath)
      df$ds <- as.Date(df$ds, '%m/%d/%y')
      df
    }
  })
  
  df2 <- reactive({
    if (input$log_trans) {
      df2 <- df()
      df2$y <- log(df2$y)
      df2
    } else {
      df()
    }
  })

  # Run model -----------------------------------------------------------------
  model_obj <- eventReactive(input$run_model, {
    m <- prophet(df2())
  })
  
  future <- reactive({
    future <- make_future_dataframe(model_obj(), periods = 365)    
  })
  
  forecast <- reactive({
    forecast <- predict(model_obj(), future()) 
  })
  
  output$forecast_plot <- renderPlot({
    plot(model_obj(), forecast())
  })
  
  output$component_plot <- renderPlot({
    prophet_plot_components(model_obj(), forecast())
  })
  
}
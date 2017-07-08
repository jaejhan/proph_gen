library(shinydashboard)
library(prophet)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
source("helpers.R")
load("data/proph_gen_ex.Rdata")

server <- function(input, output) {
  # Prepare dataset -----------------------------------------------------------
  df <- reactive({
    if (is.null(input$df)) {
      proph_gen_ex
    } else {
      inFile <- input$df
      df <- readr::read_csv(inFile$datapath)
      df$ds <- as.Date(df$ds, '%m/%d/%y')
      df
    }
  })
  
  cap_val <- reactive({
    if (input$growth == "logistic") {
      input$cap
    }
  })
  
  df_cap <- reactive({
    df_cap <- df()
    if (input$growth == "linear") {
      df_cap
    } else if (input$growth == "logistic") {
      df_cap$cap <- cap_val()
      df_cap
    }
  })
  
  df2 <- reactive({
    if (input$log_trans) {
      df2 <- df_cap()
      df2$y <- log(df2$y)
      if (any(names(df2) %in% 'cap')) {
        df2$cap <- log(df2$cap)  
      }
      
      df2
    } else {
      df_cap()
    }
  })

  # Run model -----------------------------------------------------------------
  model_obj <- eventReactive(input$run_model, {
    m <- prophet(df2(), growth = input$growth)
  })
  
  future <- reactive({
    future <- make_future_dataframe(model_obj(), periods = 365) 
    
    if (any(names(df2()) %in% 'cap')) {
      if (input$log_trans) {
        future$cap <- log(cap_val()) 
      } else {
        future$cap <- cap_val() 
      }
    }
    
    future
  })
  
  forecast <- reactive({
    forecast <- predict(model_obj(), future()) 
  })
  
  df_plot <- reactive({
    df_plot <- df_for_plotting(model_obj(), forecast())
  })
  
  output$forecast_plot <- renderPlot({
    plot(model_obj(), forecast())
  })
  
  output$trend_plot <- renderPlot({
    plot_trend(df_plot())
  })
  
  output$yearly_plot <- renderPlot({
    plot_yearly(model_obj())
  })
  
  output$weekly_plot <- renderPlot({
    plot_weekly(model_obj())
  })
  
}
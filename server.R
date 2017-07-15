library(shinydashboard)
library(prophet)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
source("helpers.R")
load("data/proph_gen_ex.Rdata")
load("data/proph_gen_holiday_ex.Rdata")

server <- function(input, output) {
  # Prepare main dataset -----------------------------------------------------------
  df <- reactive({
    if (is.null(input$df)) {
      proph_gen_ex
    } else {
      in_file <- input$df
      df <- readr::read_csv(in_file$datapath)
      df$ds <- as.Date(df$ds, '%m/%d/%y')
      df
    }
  })
  
  cap_val <- eventReactive(input$run_model, {
    if (input$growth == "logistic") {
      input$cap
    }
  })
  
  df_cap <- eventReactive(input$run_model, {
    df_cap <- df()
    if (input$growth == "linear") {
      df_cap
    } else if (input$growth == "logistic") {
      df_cap$cap <- cap_val()
      df_cap
    }
  })
  
  df2 <- eventReactive(input$run_model, {
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
  

  # Prepare holidays dataset ---------------------------------------------------
  holiday_df <- reactive({
    if (is.null(input$df) & input$holiday_effect == "yes") {
      proph_gen_holiday_ex
    } else if (is.null(input$df) & input$holiday_effect == "no") {
      NULL
    } else if (!is.null(input$holiday_df) & input$holiday_effect == "yes") {
      in_file_hol <- input$holiday_df
      holiday_df <- readr::read_csv(in_file_hol$datapath)
      holiday_df$ds <- as.Date(holiday_df$ds, '%m/%d/%y')
      holiday_df
    } else {
      NULL
    }
  })
  

  # Run model -----------------------------------------------------------------
  model_obj <- eventReactive(input$run_model, {
    m <- prophet(df = df2(), 
                 growth = input$growth, 
                 holidays = holiday_df(), 
                 changepoint.prior.scale = input$changept_prior_scale)
  })
  
  future <- eventReactive(input$run_model, {
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
  
  forecast <- eventReactive(input$run_model, {
    forecast <- predict(model_obj(), future()) 
  })
  
  df_plot <- eventReactive(input$run_model, {
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
  
  output$holiday_plot <- renderPlot({
    input$run_model
    
    if (isolate(input$holiday_effect) == "yes") {
      plot_holidays(model_obj(), df_plot())
    } else {
      ggplot() +
        ggtitle("No holiday effect")
    }
    
  })
  
}
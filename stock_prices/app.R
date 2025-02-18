# install.packages(c("shiny", "shinydashboard", "tidyverse", "tidymodels", "prophet", "lubridate", "modeltime", "timetk", "patchwork", "xgboost", "plotly"))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)
library(prophet)
library(lubridate)
library(modeltime)
library(timetk)
library(patchwork)
library(xgboost)
library(plotly)

theme_set(theme_light())

googlesheets4::gs4_deauth()

set.seed(05262022)

jetblue <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1SpRXsC3kXDaQLUfC6cPIOvsqxDF6updhgHRJeT8PTog/edit#gid=0') %>% 
  janitor::clean_names() %>% 
  mutate(ds = as_date(date)) %>% 
  select(-date)

# Define UI for application that draws a histogram
ui <- 
  navbarPage("Stock Forecasting",
             tabPanel('Prophet Training',
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          
                          selectInput(inputId = 'day_season',
                                      label = 'Daily Seasonality',
                                      choices = c('auto', TRUE, FALSE),
                                      selected = 'auto'),
                          
                          selectInput(inputId = 'week_season',
                                      label = 'Weekly Seasonality',
                                      choices = c('auto', TRUE, FALSE),
                                      selected = 'auto'),
                          
                          selectInput(inputId = 'year_season',
                                      label = 'Yearly Seasonality',
                                      choices = c('auto', TRUE, FALSE),
                                      selected = 'auto'),
                          
                          selectInput(inputId = 'changepoint_prior_scale',
                                      label = 'Changepoint value (default = .05)',
                                      choices = c(seq(.001, .5, by = .01), .05),
                                      selected = .05),
                          
                          selectInput(inputId = 'seasonality_prior_scale',
                                      label = 'Seasonality value (default = 10)',
                                      choices = c(seq(.01, 10, by = .1), 10),
                                      selected = 10),
                          
                          selectInput(inputId = 'holidays_prior_scale',
                                      label = 'Holiday value (default = 10)',
                                      choices = c(seq(.01, 10, by = .1), 10),
                                      selected = 10),
                          
                          selectInput(inputId = 'seasonality_mode',
                                      label = 'Seasonality mode (default = additive)',
                                      choices = c('additive', 'multiplicative'),
                                      selected = 'additive'),
                          
                          actionButton('Update',
                                       label = 'Update Training Model')
                        ),
                        mainPanel(
                          fluidRow(box(plotOutput('jet_train_cast'),
                                       title = 'JetBlue Model Training'),
                                   box(dataTableOutput('train_rmse'),
                                       title = 'JetBlue Train Model Accuracy')))
                        )
                      ),
             tabPanel('JetBlue Forecast',
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          
                        selectInput(inputId = 'future_forecast',
                                    label = 'Months to Forecast Ahead',
                                    choices = seq(1, 24, by = 1),
                                    selected = 1),
                        
                        actionButton('Forecast',
                                     label = 'Update Forecast')
                      ),
                      mainPanel(
                        fluidRow(
                          box(plotOutput('jetblue_forecast'),
                              title = 'JetBlue Testing Model'),
                          box(plotOutput('jet_future_forecast'),
                              title = 'Forecasted JetBlue Stock'),
                          box(dataTableOutput('jet_test_rmse'),
                              title = 'Jetblue Model Accuracy')),
                          )
             )
             )
)

server <- 
  function(input, output){
  
    set.seed(05262022)
    
    # making training/testing datasets
    jet_splits <- reactive({
      initial_time_split(jetblue)
    })
    
    # make choosing model parameters reactive
    day_sea <- eventReactive(input$Update,
                  input$day_season)
    week_sea <- eventReactive(input$Update,
                  input$week_season)
    year_sea <- eventReactive(input$Update,
                  input$year_season)
    cp_select <- eventReactive(input$Update,
                               input$changepoint_prior_scale)
    season_select <- eventReactive(input$Update,
                                   input$seasonality_prior_scale)
    holi_select <- eventReactive(input$Update,
                                 input$holidays_prior_scale)
    sea_mode_select <- eventReactive(input$Update,
                                     input$seasonality_mode)
    
    # model characteristics
    jet_model_re <- reactive({
      prophet_reg(
        seasonality_yearly = year_sea(),
        seasonality_weekly = week_sea(),
        seasonality_daily = day_sea(),
        prior_scale_changepoints = as.numeric(cp_select()),
        prior_scale_seasonality = as.numeric(season_select()),
        prior_scale_holidays = as.numeric(holi_select()),
        season = sea_mode_select()) %>% 
        set_engine(engine = 'prophet',
                   num.threads = parallel::detectCores(),
                   verbose = TRUE)
    })
    
    # fitting model to training data
    jet_train_fit_re <- reactive({
      jet_model_re() %>% 
        fit(close ~ ds,
           data = training(jet_splits()))
    })
    
    # visuals of training model fit
    output$jet_train_cast <- renderPlot({
      jet_train_fit_re() %>% 
        modeltime_calibrate(new_data = training(jet_splits())) %>% 
        modeltime_forecast(new_data = training(jet_splits()),
                           actual_data = jetblue) %>% 
        plot_modeltime_forecast(.interactive = FALSE)
      
    })
    
    # RMSE of training model
    output$train_rmse <- renderDataTable({
      jet_train_rmse <-
        jet_train_fit_re() %>% 
        modeltime_calibrate(new_data = training(jet_splits())) %>% 
        modeltime_accuracy() %>% 
        pluck('rmse')
      
      data.frame(Stock = 'JetBlue',
                 RMSE = jet_train_rmse)
    })

    # try to make training input reactive for testing model

    # testing jet models
    output$jetblue_forecast <- renderPlot({
      jet_train_fit_re() %>% 
        modeltime_calibrate(new_data = testing(jet_splits())) %>% 
        modeltime_forecast(new_data = testing(jet_splits()),
                           actual_data = jetblue) %>% 
        plot_modeltime_forecast(.interactive = FALSE)
    })
    
    #RMSE of jetblue models (prophet)
    output$jet_test_rmse <- renderDataTable({
      jet_test_rmse <- jet_train_fit_re() %>% 
        modeltime_calibrate(new_data = testing(jet_splits())) %>%
        modeltime_accuracy() %>% 
        pluck('rmse')
      
      data.frame(model = 'Prophet',
                 RMSE = jet_test_rmse) 
    })
    
    forecast_select <- eventReactive(input$Forecast,
                                     as.character(input$future_forecast))
    
    output$jet_future_forecast <- renderPlot({
      
      cali_mod <- jet_train_fit_re() %>% 
        modeltime_calibrate(new_data = testing(jet_splits()))
      
      refit_mod <- cali_mod %>% 
        modeltime_refit(data = jetblue)
      
      refit_mod %>% 
        modeltime_forecast(h = paste0(forecast_select(), ' months'),
                           actual_data = jetblue) %>% 
        plot_modeltime_forecast(.interactive = FALSE)
    })
    
}

shinyApp(ui, server)

#1. Use arima_boost(), exp_smoothing(), prophet_reg() models;
#2. Compare RMSE scores on test set;
#3. Make forecast on lowest RMSE score model;
#4. Visualize past data and forecast values on one plot; make separation with two different colors.

library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(data.table)
library(rstudioapi)
library(skimr)
library(car)
library(h2o)
library(rlang)
library(glue)
library(highcharter)
library(lime)
library(inspectdf)
library(caret)
library(glue)
library(scorecard)
library(mice)
library(plotly)
library(recipes) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(inspectdf)
path <- dirname(getSourceEditorContext()$path)
setwd(path)

raw <- fread("AirPassengers.csv")

colnames(raw) <- c('Date','Count')

raw %>% glimpse()
raw %>% inspect_na()
raw$Date <- raw$Date  %>% paste0('-01')

raw$Date <- raw$Date  %>% as.Date()

raw$Count <- raw$Count  %>% as.numeric()

library(tidymodels)
library(modeltime)
library(timetk)

splits <- initial_time_split(raw, prop = 0.9)

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Count ~ Date,  data = training(splits))


model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Count ~ Date, data = training(splits))


model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Count ~ Date, data = training(splits))

recipe_spec <- recipe(Count ~ Date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_ets,
  model_fit_prophet
)

models_tbl


calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = TRUE
  )

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = raw)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = raw) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

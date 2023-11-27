library(tidyverse)
library(patchwork)
library(timetk)
library(vroom)
library(tidymodels)

## Read in Data
test <- vroom('test.csv')
train <- vroom('train.csv')

# Create data sets filtered to a random store and item

store1_item3 <- train1_3 <- train %>%
  filter(store == 1 & item == 3)

store5_item7 <- train5_7 <- train %>%
  filter(store == 5 & item == 7)

store6_item17 <- train %>%
  filter(store == 6 & item == 17)

store9_item22 <- train %>%
  filter(store == 9 & item == 22)

# Autocorrelation Function Plots

p1 <- store1_item3 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 1 Item 3")

p2 <- store5_item7 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 5 Item 7")

p3 <- store6_item17 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 6 Item 17")


p4 <- store9_item22 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365) +
  ggtitle("Store 9 Item 22")

(p1 + p2) / (p3 + p4)



# Create Recipe -----------------------------------------------------------

my_recipe <- recipe(sales~date, data = train1_3) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_date(date, features = "quarter") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

prep <- prep(my_recipe)
baked <- bake(prep, train1_3)

# Random Forest -----------------------------------------------------------

forest_mod <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# set workflow
forest_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(forest_mod)

## Grid of tuning values
tuning_grid <- grid_regular(mtry(range = c(1,9)),
                            min_n(),
                            levels = 5)

# split data into folds
folds <- vfold_cv(train1_3, v = 10, repeats = 1)

# run Cross validation
CV_results <- forest_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, smape))

# find best parameters
bestTune <- CV_results %>%
  select_best("smape")

# collect metrics
collect_metrics(CV_results) %>%
  filter(bestTune) %>%
  pull(mean)

final_forest_workflow <- forest_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data = amazon_train)

# predict
forest_preds <- predict(final_forest_workflow,
                        new_data = amazon_test,
                        type = "prob")

final_forest_preds <- tibble(id = amazon_test$id,
                             ACTION = forest_preds$.pred_1)

vroom_write(final_forest_preds, "forest_predictions.csv", delim = ",")


# Exponential Smoothing ---------------------------------------------------

library(modeltime)
library(timetk)

cv_split <- time_series_split(train5_7, assess = '3 months', cumulative  = TRUE)

# Set up model
es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data = training(cv_split))

# Cross_validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

# Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train5_7) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast

es_fullfit <- cv_results %>%
  modeltime_refit(data = train5_7)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = '3 months') %>%
  rename(date = .index, sales = .value) %>%
  select(date, sales) %>%
  full_join(., y = test, by = 'date') %>%
  select(id, sales)

p4 <- es_fullfit %>%
  modeltime_forecast(h = '3 months', actual_data = train5_7) %>%
  plot_modeltime_forecast(.interactive = FALSE)

plotly::subplot(p1, p2, p3, p4, nrows = 2)


# ARIMA Model -------------------------------------------------------------

library(forecast)

arima_recipe <- recipe(sales~date, data = train) %>%
  step_date(date, features = c("dow", "month", "year"))

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar = 5,
                         non_seasonal_ma = 5,
                         seasonal_ar = 2,
                         seasonal_ma = 2,
                         non_seasonal_differences = 2,
                         seasonal_differences = 2) %>%
  set_engine("auto_arima")

cv_split <- time_series_split(train5_7, assess = '3 months', cumulative  = TRUE)

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

# Visualize CV results
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train5_7) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast

arima_fullfit <- cv_results %>%
  modeltime_refit(data = train5_7)

arima_preds <- arima_fullfit %>%
  modeltime_forecast(h = '3 months') %>%
  rename(date = .index, sales = .value) %>%
  select(date, sales) %>%
  full_join(., y = test, by = 'date') %>%
  select(id, sales)

p2 <- arima_fullfit %>%
  modeltime_forecast(h = '3 months', actual_data = train5_7) %>%
  plot_modeltime_forecast(.interactive = FALSE)

plotly::subplot(p1, p2, p3, p4, nrows = 2)

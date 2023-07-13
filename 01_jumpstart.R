# 1.0 Goal of Project ----

# Forecast Daily Email Users - Next 8-WEEKS 




# 2.0 Libraries Needed for Analysis ----


# Time Series Machine Learning

library(tidymodels)
# Modeltime is used to integrate time series models into the tidymodels ecosystem
# Modeltime models return parsnip model objects
library(modeltime)


# Exploratory Data Analysis

library(DataExplorer)


# Core Libraries

library(tidyverse)
library(timetk)
library(lubridate)




# 3.0 Data Used for Analysis -----


# Here we have a 23,672 x 10 tibble
# This is a database export from Mailchimp that contains raw opt-in data
# Often you get data that is not a tidy time series. This data is at user-level. Will need to be prepared before we can analyze it
# It's important to note that there are four columns with timestamp data included
# Each of those columns is formatted as YYYY-MM-DD
# We can use one of those four columns as our time variable for a time series analysis
mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")




# 4.0 Exploratory Data Analysis & Data Preparation ----


# glimpse() is a great way to view wide datasets
# You get an understanding of what features exist and what data types they are stored as
mailchimp_users_tbl %>%
  glimpse()


# * Turning raw data into a time series format ----


# Here we are doing a count of opt-ins by day
# We are turning our raw data into a time series format
optins_day_tbl <- mailchimp_users_tbl %>%
  # summarize_by_time() comes from the timetk package
  # It's for aggregating time series using time stamps and value
  # Works like summarise() but for time series
  summarise_by_time(
    # .date_var is going to be the date column we want to summarize
    # In this case we are going to use the column "optin_time"
    .date_var = optin_time,
    # This is the metric for how we will summarize our timestamp data
    # We are picking "day" for this example, but could be month or year
    .by = "day",
    # We want to summarize the number of observations per day
    # n() performs a frequency count
    # We will create a new column called "optins" that will contain that summarization
    # So now we have a 608 x 2 tibble with the timestamp and then the number of observations per that timestamp
    # For example, on 2018-07-03 there were 10 people that opted in to receiving emails
    optins = n()
    )


# Here we are doing a count of opt-ins by month
optins_month_tbl <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "month",
    # So now we have a 22 x 2 tibble with the timestamp and then the number of observations per that timestamp
    optins = n()
  )


# Here we are doing a count of opt-ins by year
optins_year_tbl <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "year",
    # So now we have a 3 x 2 tibble with the timestamp and then the number of observations per that timestamp
    optins = n()
  )


# * Summarizing our time series data ----


# Here we are viewing an in-depth summary of the time series DAILY data we just created
optins_diagnostics <- optins_day_tbl %>%
  # tk_summary_diagnostics provides a convenient summary of your time series
  # Remember that in time series the index is basically the name of the date/date-time column
  # Number of observations: 608
  # When the time series starts: 2018-06-08
  # When the time series ends: 2020-03-02
  # What unit the time series is expressed in: days
  # What scale the time series has: day (meaning ONE day between each period)
      ## scale is the most common difference between timestamps within a time series
      ## This is also called "interval", "frequency", "period"
  # What time zone the data is recorded in: UTC
      ## time zone is most important for date-time data that varies by hour of the day
  # Each diff column characterizes the interval between time stamps in seconds
      ## one day = 86,400 seconds
      ## An irregular time series occurs when the difference between time stamps is not constant
      ## This happens when data is missing or the interval is non-constant (like how some months have different number of days)
      ## We should try and standardize our irregular time series
  tk_summary_diagnostics(.date_var = optin_time)


# * Standardizing  our time series data ----


# Here we will now try to standardize the time series data we created
# You can tell there's irregularity based on diff.maximum in our summary diagnostics, which was 2,160,000 seconds
optins_day_prepared_tbl <- optins_day_tbl %>%
  # pad_by_time comes from the timetk library
  # It performs time series padding
  # It fills in any gaps to convert the time series to a regular time series
  # So now we have the time stamps filled in, but the column for "optins" now has a bunch of NA values
  # We can fix that by adding in .pad_value and in this case we will make the value 0
  # This makes sense since there are no opt-ins for those filled in days
  # We now have a 634 x 2 tibble
  pad_by_time(.date_var = optin_time, 
              .by = "day",
              .pad_value = 0)


# Here we are checking the diagnostics on the standardized time series data
# There are now no longer any huge gaps in time
# Everything has a difference of 86,400 seconds, which is equal to one day
# That is perfect for a DAILY time series
optins_prepared_diagnostics <- optins_day_prepared_tbl %>%
  tk_summary_diagnostics(.date_var = optin_time)


# * Visualizing our time series data ----


# Here we will visualize our time series data
optins_ts_plot <- optins_day_prepared_tbl %>%
  # plot_time_series() comes from the timetk library
  # It's a way to visualize a time series interactively
  # optin_time will be our x-axis, and optins will be our y-axis
  # There is a HUGE spike in optins on 2018-11-19
  plot_time_series(optin_time, optins)




# 5.0 Evaluation Period ----


# * Explanation of evaluation period ----


# An evaluation period or window is the span of the time series that we want to include in our train/test sets
# It's often common to isolate just a specific period of time for training that is most representative of the history
# Pros of filtering: More recent data is typically more representative of trends
# Cons of filtering: Lose history or previous trends, which can be harmful if long-term trends should be included


# * Filtering our time series data ----


# Here we will filter our prepared time series data
optins_evaluation_tbl <- optins_day_prepared_tbl %>%
  # filter_by_time() comes from the timetk library
  # Simplifies the filtering process for consecutive time windows
  filter_by_time(
    .date_var = optin_time,
    # We are going to select 2018-11-20 as our start date
    # Remember that 2018-11-19 is the date with the huge spike in traffic
    .start_date = "2018-11-20",
    # Using "end" is just a shortcut to say the rest of the data after the start date
    # We now get a 469 x 2 tibble
    .end_date = "end"
  )



# * Visualizing our filtered time series data ----


# Here we are plotting our filtered time series
# We have indeed removed that giant spike from the data
optins_evaluation_tbl %>%
  plot_time_series(optin_time, optins)


# * Splitting our filtered data into training and test sets ----


# Here we will divide our filtered time series into a training and test set
optins_splits <- optins_evaluation_tbl %>%
  # time_series_split() comes from the timetk library
  # It splits the time series into training and test sets
  # The test set in time series should come from the most recent data
  time_series_split(
    date_var = optin_time,
    # Here we are going to take our last eight weeks worth of data
    assess = "8 week",
    # The default is cumulative = FALSE, which grabs just the first five observations
    # Making it TRUE grabs the rest of the data that isn't in the test set
    cumulative = TRUE
  )


# * Visualizing our train/test splits ----


# Here we are preparing our train/test for visualization
optins_splits  %>%
  # tk_time_series_cv_plan() generates a single data frame that is prepared for visualization
  # We have a 469 x 4 tibble
  tk_time_series_cv_plan() %>%
  # plot_time_series_cv_plan() generates a visualization from the tk_time_series_cv_plan()
  # We can clearly see the training set in blue and using more of the past data
  # The test set is in red and using more of the recent data
  plot_time_series_cv_plan(optin_time, optins)




# 6.0 Forecasting Using Prophet ----


# * Fitting Prophet model using both modeltime and parsnip ----


# Here we are going to fit a prophet_model to our training set
# This will become a parsnip model object
# Remember that you FIT the model first, THEN forecast
# prophet_reg() stands for "prophet regression" and comes from modeltime
# It is the interface for prophet regression modeling
model_prophet_fit <- prophet_reg() %>%
  # set_engine() is used to pick the underlying prophet implementation
  # Using "prophet" connects to prophet::prophet()
  set_engine("prophet") %>%
  # fit() is a general method used to fit models
  # For parsnip objects, fit() uses underlying function parsnip::fit.model_spec()
  # fit.model_spec() has a formula interface and uses "data" to specify training data
  # Our target value is "optins" so that is what we are trying to predict or forecast
  # All modeltime models require a date feature as a predictor
  # training() extracts the training set from the optins_splits object we created
  # So optins is a function of optin_time hence the tilde ~
  fit(optins ~ optin_time, data = training(optins_splits))


# modeltime_table() comes from the modeltime library
# It starts the forecasting workflow by organizing one or more trained parsnip models or workflow objects into a descriptive table
# So now we have a 1 x 3 tibble
prophet_model_tbl <- modeltime_table(model_prophet_fit)


# * Calibrating our Prophet model  ----


prophet_calibration_tbl <- prophet_model_tbl %>%
  # modeltime_calibrate() performs model prediction against a hold out (test set)
  # So now we have a 1 x 5 tibble
  # We've added two columns to model_tbl
  # We added .type and .calibration_data
  # The .calibration is the testing data being pulled in. It's a 56x4 tibble itself
  modeltime_calibrate(new_data = testing(optins_splits))


# * Visualizing our Prophet model forecast  ----


# Here we are visualizing the forecast from the calibrated data
prophet_calibration_tbl %>%
  # modeltime_forecast creates a dataframe with forecasted values
  # We get a 525 x 7 tibble
  # The .calibration_data column has been turned into a prediction
  modeltime_forecast(actual_data = optins_evaluation_tbl) %>%
  # plot_modeltime_forecast visualizes the output of the modeltime_forecast()
  # So now we have a plot of the actual data and then the forecast laid on top of it
  # It is certainly not the best forecast
  plot_modeltime_forecast()


# * Accuracy of our Prophet model forecast  ----


# Here we are checking the accuracy of our forecast
prophet_forecast_metrics <- prophet_calibration_tbl %>%
  # modeltime_accuracy() returns the accuracy metrics
  # so now we have a 1 x 9 tibble with columns like MAE, RMSE, and R-Squared
  modeltime_accuracy()




# 5.0 Forecasting Using Workflows ----


## Forecasting Workflow leverages tidymodles modeling tools


# * Viewing seasonality in our time series data  ----


# Here we will take a look at any seasonality within our time series
# Seasonality is a cyclic frequency in the data
optins_seasonality <- optins_evaluation_tbl %>%
  # plot_seasonal_diagnostics() comes from the timetk library
  # It visualizes common seasonalities in a time series via box plots
  # weekday, week, month, quarter, but NOT year apparently 
  # For this instance there seems to be some outliers
  plot_seasonal_diagnostics(optin_time, optins)


# Here we are looking at seasonality again but with a log transformation
# We are applying log transformation to account for the outliers in our data
optins_seasonality_log <- optins_evaluation_tbl %>%
  # We see that the most optins occur on Wednesday
  plot_seasonal_diagnostics(optin_time, log(optins))


# * Setting up recipe pipeline  ----


# Before we do any official linear regression we have to create the features necessary
# recipe() initiates a preprocessing pipeline called a "recipe"
# We can add steps to these to perform common preprocessing transformations
# We have one outcome and one predictor (makes sense since we have just two columns)
# So, optins is a function of all other variables (hence the period)
# In this case we have just one independent variable and that's our time variable
ts_recipe_1 <- recipe(optins ~ ., data = training(optins_splits)) %>%
  # So here we are adding a recipe step
  # We already stated what our model relationship is
  # step_timeseries_signature() comes from timetk
  # Adds a number of calendar features that can help in feature engineering
  # So we have one outcome, one predictor, and one operation at this point
  step_timeseries_signature(optin_time) 
  
  
# Here we are just reviewing the recipe pipeline we initialized above
ts_recipe_1 %>%
  # prep() performs any pre-calculations and preprocessing training steps
  # In this instance it officially adds the training set data into the pipeline
  # You see under Operations that there is "trained" added
  prep() %>%
  # juice() returns the training data post transformation so we can see the transformations that were applied
  # So it applies the model recipe to the training set
  # We have a 413 x 29 tibble
  juice() %>%
  # glimpse() allows us to see all the new features that were added
  glimpse()


# So now we are going to clean our time series even more
# After looking at initially we had a 413 x 29 tibble
# There are probably some unnecessary columns
ts_recipe_2 <- recipe(optins ~ ., data = training(optins_splits)) %>%
  step_timeseries_signature(optin_time) %>%
  # So here we are adding a second step to our recipe
  # We already have step_timeseries_signature()
  # step_rm() comes from the recipes package
  # It's used for removing feature preprocessing columns
  # ends_with() is a Tidyselect selector that selects any columns ending with the text you say
  step_rm(ends_with(".iso"), ends_with(".xts"),
          # contains() is a Tidyselect selector that selects any columns with the text you say
          # So after this step we have dwindled the features down to 20 after initially having 29
          contains("hour"), contains("minute"), contains("second"), contains("am.pm")) %>%
  # Here we are adding a third step to our recipe
  # step_normalize() comes from the recipes package
  # It subtracts the mean (centers) and scales to a standard deviation of 1
  # NOT SURE WHY WE ARE CHOOSING THESE COLUMNS TO NORMALIZE
  step_normalize(ends_with("index.num"), ends_with("_year")) %>%
  # step_dummy() comes from the recipes package
  # It creates dummy variables from categorical data
  # all_nominal() selects any features that are categorical (character or factor)
  # You can tell when using glimpse() how the columns are coded (date, dbl, ord, int)
  # Our full recipe is now set up
  step_dummy(all_nominal())


# Here I am just checking officially what we added to our recipe
ts_recipe_2 %>%
  prep() %>%
  # We have a 413 x 20 tibble now after the cleaning we just did above
  juice() %>%
  glimpse()


# * Initializing machine learning model for forecast  ----


# linear_reg() initializes a linear regression model specification
# It comes from the parsnip package
workflow_lr_model <- linear_reg() %>%
  # We set engine to "lm" since that's what stands for Linear Regression
  set_engine("lm")


# workflow() combines both a model and a preprocessing recipe
workflow_fit_lm <- workflow() %>%
  # add_model() adds a machine learning specification to a workflow
  # Comes from the workflows package
  # We are going to use workflow_lr_model, which is the linear regression model we initialized above
  add_model(workflow_lr_model) %>%
  # add_recipe() adds a preprocessing specification (recipe) to a workflow
  # We will use ts_recipe_2 that we created above
  # Remember that we added four steps to our recipe
  add_recipe(ts_recipe_2) %>%
  # Calling fit() on a workflow uses workflows::fit.workflow()
  # Just provide the data that you want to fit
  # Here we are going to fit our training set
  fit(training(optins_splits))


# * Comparing both forecasts  ----


# So here we are combining both the prophet model and workflow model so we can compare their performance adequately
combined_calibration_tbl <- modeltime_table(
  model_prophet_fit,
  workflow_fit_lm
) %>%
  # The fitted models have already been trained on the training set
  # Now we are incorporating (calibrating) the models on the test set
  modeltime_calibrate(testing(optins_splits))


# Here we are comparing the accuracy between our two models
# It looks like the linear regression model is performing better than the prophet model
# Remember we want lower MAE, RMSE, and higher R-Squared
combined_model_metrics <- combined_calibration_tbl %>%
  modeltime_accuracy()


# * Visualizing both forecasts  ----


# Here we will use a forecast on our test set
combined_forecast_plot <- combined_calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(optins_splits),
    # Remember we want to compare the forecast to the actual data
    # The actual data is our optins_evaluation_tbl
    actual_data = optins_evaluation_tbl
  ) %>%
  # So now we have a nice plot that shows the real data and the forecasts from our two models
  # There are some spikes in the actual data that our forecasts cannot capture
  plot_modeltime_forecast()







# 5.0 SUMMARY & NEXT STEPS ----

# We created two models: model_prophet_fit and workflow_fit_lm


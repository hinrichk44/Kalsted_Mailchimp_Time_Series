# 1.0 Goal of Project ----

# Forecast Daily Email Users - Next 8-WEEKS 




# 2.0 Libraries Needed for Time Series Analysis ----


# * Time Series Machine Learning ----


# tidymodels is a meta-package that installs packages that you need for modeling and machine learning
# Contains rsample, which provides infrastructure for efficient data splitting and resampling
# Contains parsnip, which is a tidy, unified interface to models that can be used to try a range of models without getting bogged down in the syntactical minutiae of the underlying packages.
# Contains recipes, which is a tidy interface to data pre-processing tools for feature engineering
# Contains workflows, which bundles your pre-processing, modeling, and post-processing together
# Contains tune, which helps you optimize the hyperparameters of your model and pre-processing steps
# Contains yardstick, which measures the effectiveness of models using performance metrics
## One of the biggest differences when working with multiclass problems is that your performance metrics are different. 
## The yardstick package provides implementations for many multiclass metrics.
# Contains broom, which converts the information in common statistical R objects into user-friendly, predictable formats
# Contains dials, which creates and manages tuning parameters and parameter grids
library(tidymodels)


# modeltime is used to integrate time series models into the tidymodels ecosystem
# modeltime models return parsnip model objects
# modeltime incorporates both the prophet package and forecast package
library(modeltime)


# * Exploratory Data Analysis ----


# DataExplorer aims to automate most of data handling and visualization (EDA), so that users could focus on studying the data and extracting insights.
library(DataExplorer)


# * Core Libraries ----


# tidyverse is a collection of R packages designed for data science.
# Contains ggplot2 for data visualization.
# Contains dplyr, for data manipulation.
# Contains tidyr, for data tidying.
# Contains readr, for data import.
# Contains purrr, for functional programming.
# Contains tibble, for tibbles, a modern re-imagining of data frames.
# Contains stringr, for strings.
# Contains forcats, for factors.
library(tidyverse)


# * Basic Time Series Libraries ----


# timetk is used for visualizing, wrangling, and preparing time series data
# timetk isn't really used for time series modeling itself
library(timetk)

# Lubridate is a package for working with date-time data in R, such as parsing, extracting, updating, and manipulating date-time objects and time-spans. 
library(lubridate)




# 3.0 Data Used for Time Series Analysis -----


# Here we have a 23,672 x 10 tibble from MailChimp that contains the email list of Business Data Science users
# This is a database export from MailChimp that contains raw opt-in data
# Each row represents someone who is a Business Data Science user and when they opted-in to receiving emails
# Often you get data that is not a tidy time series. This data is at user-level. Will need to be prepared before we can analyze it
# It's important to note that there are four columns with timestamp data included (optin_time, confirm_time, last_changed, date_added)
# Each of those columns is formatted as YYYY-MM-DD
# We can use one of those four columns as our time variable for a time series analysis
# So basically we can take this typical dataframe and transform it into a usable time series dataset
mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")




# 4.0 Exploratory Data Analysis & Data Preparation for Time Series Analysis ----


# * Turning raw data into a time series format ----


# Here we are doing a count of email opt-ins by day
# "How many Business Data Science users are opting-in each day?"
# We are turning our raw data into a time series format
mailchimp_optins_daily <- mailchimp_users_tbl %>%
  # summarize_by_time() comes from the timetk package
  # It's for aggregating time series using time stamps and value
  # Works like summarise() but for time series
  summarise_by_time(
    # .date_var is going to be the date column we want to summarize
    # In this case we are going to use the column "optin_time"
    # optin_time is the date the user opted in to receiving emails from Business Data Science 
    .date_var = optin_time,
    # This is the metric for how we will summarize our timestamp data
    # We are picking "day" for this example, but could be month or year
    .by = "day",
    # We want to summarize the number of observations per day
    # n() performs a frequency count
    # We will create a new column called "daily_optins" that will contain that summarization
    # So now we have a 608 x 2 tibble with the timestamp (optin_time) and then the number of observations per that timestamp (daily_optins)
    # For example, on 2018-07-03 there were 10 people that opted in to receiving emails
    daily_optins = n()
    )


# Here we are doing a count of opt-ins by week
mailchimp_optins_weekly <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "week",
    # So now we have a 89 x 2 tibble with the timestamp and then the number of observations per that timestamp
    weekly_optins = n()
  )


# Here we are doing a count of opt-ins by month
mailchimp_optins_monthly <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "month",
    # So now we have a 22 x 2 tibble with the timestamp and then the number of observations per that timestamp
    monthly_optins = n()
  )


# Here we are doing a count of opt-ins by year
mailchimp_optins_yearly <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "year",
    # So now we have a 3 x 2 tibble with the timestamp and then the number of observations per that timestamp
    yearly_optins = n()
  )


# * Summarizing our time series data ----


# Here we are viewing an in-depth summary of the time series DAILY data we just created
mailchimp_optins_daily_diagnostics <- mailchimp_optins_daily %>%
  # tk_summary_diagnostics comes from provides a convenient summary of your time series
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


# Here is our time series summary using tk_summary_diagnostics()
# So as you can see in the diff.mean and diff.maximum columns they are not standard to 86,400 seconds (one day)
# We want to make sure each observation is a day apart in the time series
# We can fill in the gaps to standardize the time series
# A tibble: 1 x 12
# n.obs   start      end          units  scale  tzone   diff.minimum   diff.q1     diff.median diff.mean   diff.q3     diff.maximum
# <int>  <date>      <date>      <chr>  <chr>  <chr>    <dbl>          <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
# 608    2018-06-08  2020-03-02  days   day    UTC      86400          86400       86400       90101.      86400       2160000
mailchimp_optins_daily_diagnostics


# * Standardizing our time series data ----


# Here we will now try to standardize the time series data we created
# You can tell there's irregularity based on diff.maximum in our summary diagnostics, which was 2,160,000 seconds
mailchimp_optins_daily_standardized <- mailchimp_optins_daily %>%
  # pad_by_time() comes from the timetk library
  # It performs time series padding
  # It fills in any gaps to convert the irregular time series into a regular time series
  # So now we have the time stamps filled in, but the column for "daily_optins" now has a bunch of NA values
  # We can fix that by adding in .pad_value and in this case we will make the value 0
  # This makes sense since there are no opt-ins for those filled in days
  # We now have a 634 x 2 tibble
  # We started with a 608 x 2 tibble so we filled in 26 missing days. 
  pad_by_time(.date_var = optin_time, 
              .by = "day",
              .pad_value = 0)


# Here we are checking the diagnostics on the standardized time series data
mailchimp_optins_daily_diagnostics_2 <- mailchimp_optins_daily_standardized %>%
  tk_summary_diagnostics(.date_var = optin_time)


# There are now no longer any huge gaps in time
# Everything has a difference of 86,400 seconds, which is equal to one day
# That is perfect for a DAILY time series
# A tibble: 1 x 12
# n.obs  start      end        units scale tzone    diff.minimum diff.q1     diff.median diff.mean diff.q3     diff.maximum
# <int> <date>     <date>     <chr> <chr> <chr>     <dbl>        <dbl>       <dbl>       <dbl>     <dbl>       <dbl>
#  634  2018-06-08 2020-03-02 days  day   UTC       86400        86400       86400       86400    86400        86400
mailchimp_optins_daily_diagnostics_2


# * Visualizing our time series data ----


# Here we will visualize our time series data
mailchimp_optins_daily_plot <- mailchimp_optins_daily_standardized %>%
  # plot_time_series() comes from the timetk library
  # It's a way to visualize a time series interactively
  # optin_time will be our x-axis, and daily_optins will be our y-axis
  plot_time_series(.date_var = optin_time, .value = daily_optins)

# Here is our time series visualization for the daily optins data
# There is a HUGE spike in optins on 2018-11-19
mailchimp_optins_daily_plot




# 5.0 Time Series Evaluation Period ----


# * Explanation of evaluation period ----


# An evaluation period or window is the span of the time series that we want to include in our train/test sets
# It's often common to isolate just a specific period of time for training that is most representative of the history
# Pros of filtering: More recent data is typically more representative of trends
# Cons of filtering: Lose history or previous trends, which can be harmful if long-term trends should be included


# * Filtering our time series data ----


# Here we will filter our prepared time series data
mailchimp_optins_daily_evaluated <- mailchimp_optins_daily_standardized %>%
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


# Here we are plotting our filtered/evaluated time series
mailchimp_optins_daily_evaluated_plot <- mailchimp_optins_daily_evaluated %>%
  plot_time_series(optin_time, daily_optins)


# Here is our visualization of the evaluation period of our time series data
# We have indeed removed that giant spike from the data
mailchimp_optins_daily_evaluated_plot



# * Splitting our filtered data into training and test sets ----


# Here we will divide our filtered time series into a training and test set
train_test_splits <- mailchimp_optins_daily_evaluated %>%
  # time_series_split() comes from the timetk library
  # It splits the time series into training and test sets
  # The test set in time series should come from the most recent data
  time_series_split(
    date_var = optin_time,
    # Here we are going to take our last eight weeks worth of data
    assess = "8 weeks",
    # The default is cumulative = FALSE, which grabs just the first five observations
    # Making it TRUE grabs the rest of the data that isn't in the test set
    cumulative = TRUE
  )


# * Visualizing our train/test splits ----


# Here we are preparing our train/test for visualization
train_test_splits_plot <- train_test_splits  %>%
  # tk_time_series_cv_plan() generates a single data frame that is prepared for visualization
  # We have a 469 x 4 tibble
  tk_time_series_cv_plan() %>%
  # plot_time_series_cv_plan() generates a visualization from the tk_time_series_cv_plan()
  plot_time_series_cv_plan(optin_time, daily_optins)


# Here is the visualization of our training and test set
# We can clearly see the training set in blue and using more of the past data
# The test set is in red and using more of the recent data
train_test_splits_plot


# 6.0 Time Series Forecasting Using Prophet ----


# * Fitting Prophet model using both modeltime and parsnip ----


# Here we are going to fit a Prophet model to our training set
# This will become a parsnip model object
# Remember that you FIT the model first, THEN forecast
# prophet_reg() stands for "prophet regression" and comes from modeltime
# In the tidymodels framework you will have things like linear_reg() or boost_tree(), which come from parsnip
# It is the interface for prophet regression modeling
model_01_ts_prophet <- prophet_reg() %>%
  # For many tidymodels you need set_mode() but apparently not for this one
  # set_engine() is used to pick the underlying prophet implementation
  # Using "prophet" connects to prophet::prophet()
  set_engine("prophet") %>%
  # fit() is a general method used to fit models
  # We will fit our model on the training set as is standard practice
  # For parsnip objects, fit() uses underlying function parsnip::fit.model_spec()
  # fit.model_spec() has a formula interface and uses "data" to specify training data
  # Our target value is "daily_optins" so that is what we are trying to predict or forecast
  # All modeltime models require a date feature as a predictor
  # training() extracts the training set from the train_test_splits object we created above
  # So daily_optins is a function of optin_time hence the tilde ~
  fit(daily_optins ~ optin_time, data = training(train_test_splits))


# Here is a rundown of the Prophet model we just set up
# parsnip model object
# PROPHET Model
# - growth: 'linear'
# - n.changepoints: 25
# - changepoint.range: 0.8
# - yearly.seasonality: 'auto'
# - weekly.seasonality: 'auto'
# - daily.seasonality: 'auto'
# - seasonality.mode: 'additive'
# - changepoint.prior.scale: 0.05
# - seasonality.prior.scale: 10
# - holidays.prior.scale: 10
# - logistic_cap: NULL
# - logistic_floor: NULL
# - extra_regressors: 0
model_01_ts_prophet


# Here we are trying to store and organize the models we create
# modeltime_table() comes from the modeltime library
# It starts the forecasting workflow by organizing one or more trained parsnip models or workflow objects into a descriptive table
# So now we have a 1 x 3 tibble
# We have .model_id which is just a number to identify by row
# We have .model, which contains our actual Prophet model we created above
# We have .model_desc which is PROPHET
model_01_ts_prophet_tbl <- modeltime_table(model_01_ts_prophet)


# * Calibrating our Prophet model  ----


# Here we are calibrating our Prophet model
# This will give us a forecast that we must unnest 
model_01_ts_prophet_calibration_tbl <- model_01_ts_prophet_tbl %>%
  # modeltime_calibrate() performs model prediction against a hold out (test set)
  # So now we have a 1 x 5 tibble
  # We've added two columns to model_01_ts_prophet_tbl
  # We added .type and .calibration_data
  # The .calibration_data is the testing data being pulled in. It's a 56x4 tibble itself
  modeltime_calibrate(new_data = testing(train_test_splits))


# * Visualizing our Prophet model forecast  ----


# Here we are extracting the forecast results from our calibrated data
model_01_ts_prophet_forecast <- model_01_ts_prophet_calibration_tbl %>%
  # modeltime_forecast() comes from modeltime and creates a dataframe with forecasted values
  # We get a 525 x 7 tibble
  # The .calibration_data column has been turned into a prediction
  # In model_desc we have "ACTUAL" for the real data and "PROPHET" for the forecasted data
  # In the .key column we have "actual" for real data and "prediction" for forecasted data
  # So, the forecast is being performed on the calibrated data
  # We are adding/overlaying that with our actual data
  # If there wasn't a pipe/tidy format you could put new_data = model_01_ts_prophet_calibration_tbl in modeltime_forecast()
  modeltime_forecast(actual_data = mailchimp_optins_daily_evaluated) 


# Here we are visualizing the forecast from the calibrated data
model_01_ts_prophet_forecast_plot <- model_01_ts_prophet_forecast %>%
  # plot_modeltime_forecast() comes from modeltime and visualizes the output of the modeltime_forecast()
  plot_modeltime_forecast()


# Here is the visualization of our forecast
# So now we have a plot of the actual data and then the forecast laid on top of it
# It is certainly not the best forecast
model_01_ts_prophet_forecast_plot


# * Accuracy of our Prophet model forecast ----


# Here we are checking the accuracy of our forecast
model_01_ts_prophet_forecast_metrics <- model_01_ts_prophet_calibration_tbl %>%
  # modeltime_accuracy() comes from modeltime and returns the accuracy metrics
  # so now we have a 1 x 9 tibble with columns like MAE, RMSE, and R-Squared
  modeltime_accuracy()




# 7.0 Time Series Forecasting With Feature Engineering ----


# * Viewing seasonality in our time series data  ----


# Here we will take a look at any seasonality within our time series
# Seasonality is a cyclic frequency in the data
mailchimp_optins_daily_seasonality_plot <- mailchimp_optins_daily_evaluated %>%
  # plot_seasonal_diagnostics() comes from the timetk library
  # It visualizes common seasonalities in a time series via box plots
  # weekday, week, month, quarter, but NOT year apparently 
  plot_seasonal_diagnostics(optin_time, daily_optins)


# Here is our visualization of seasonality within our time series data
# For this instance there seems to be some outliers
# The outliers are causing some of the graphs to seem squished and you can't get a great look at the trends
mailchimp_optins_daily_seasonality_plot


# Here we are looking at seasonality again but with a log transformation
# We are applying log transformation to account for the outliers in our data
mailchimp_optins_daily_seasonality_log_plot <- mailchimp_optins_daily_evaluated %>%
  plot_seasonal_diagnostics(optin_time, log(daily_optins))


# This is much easier to see and evaluate
# We see that the most optins occur on Wednesday
mailchimp_optins_daily_seasonality_log_plot


# * Setting up Tidymodel recipes  ----


# We created a Prophet model for time series forecasting above
# Now we are going to create a simple linear regression model for time series forecasting
# Before we do any official linear regression we have to create the features necessary
# recipe() initiates a preprocessing pipeline called a "recipe"
# We can add steps to these to perform common preprocessing transformations
# We have one outcome and one predictor (makes sense since we have just two columns)
# So, daily_optins is a function of all other variables (hence the period)
# In this case we have just one independent variable and that's our time variable
ts_recipe_1 <- recipe(daily_optins ~ ., data = training(train_test_splits)) %>%
  # So here we are adding a recipe step
  # We already stated what our model relationship is
  # step_timeseries_signature() comes from timetk
  # Adds a number of calendar features that can help in feature engineering
  # So we have one outcome, one predictor, and one operation at this point
  step_timeseries_signature(optin_time) 
  
  
# Here we are just reviewing the recipe pipeline we initialized above
ts_recipe_1_juiced <- ts_recipe_1 %>%
  # prep() performs any pre-calculations and preprocessing training steps
  # In this instance it officially adds the training set data into the pipeline
  # You see under Operations that there is "trained" added
  prep() %>%
  # juice() returns the training data post transformation so we can see the transformations that were applied
  # So it applies the model recipe to the training set
  # We have a 413 x 29 tibble
  juice() 


ts_recipe_1_juiced %>%
  # glimpse() allows us to see all the new features that were added in an easier fashion
  glimpse()


# So now we are going to clean our time series even more
# After looking at our first recipe we initially had a 413 x 29 tibble
# There are probably some unnecessary columns/features for our model that we can get rid of
ts_recipe_2 <- recipe(daily_optins ~ ., data = training(train_test_splits)) %>%
  step_timeseries_signature(optin_time) %>%
  # So here we are adding a second step to our recipe
  # We already have step_timeseries_signature()
  # step_rm() comes from the recipes package
  # It's used for removing feature preprocessing columns
  # ends_with() is a Tidyselect selector that selects any columns ending with the text you say
  # So we're removing columns like optin_time_week.iso and optin_time_wday.xts
  # So after this step we have dwindled the features down to 20 after initially having 29
  step_rm(ends_with(".iso"), 
          ends_with(".xts"),
          # contains() is a Tidyselect selector that selects any columns with the text you say
          # We are removing hour/minute/second because we don't have that data in the first place
          # We have month, day, year but not an actual time in our data
          contains("hour"), 
          contains("minute"),
          contains("second"), 
          contains("am.pm")) %>%
  # Here we are adding a third step to our recipe
  # step_normalize() comes from the recipes package
  # It subtracts the mean (centers) and scales to a standard deviation of 1
  # NOT SURE WHY WE ARE CHOOSING THESE COLUMNS TO NORMALIZE
  step_normalize(ends_with("index.num"), 
                 ends_with("_year")) %>%
  # step_dummy() comes from the recipes package
  # It creates dummy variables from categorical data
  # all_nominal() selects any features that are categorical (character or factor)
  # You can tell when using glimpse() how the columns are coded (date, dbl, ord, int)
  # Our full recipe is now set up
  step_dummy(all_nominal())


# Here I am just checking officially what we added to our recipe
ts_recipe_2_juiced <- ts_recipe_2 %>%
  prep() %>%
  # We have a 413 x 35 tibble now after the cleaning we just did above
  # So we actually ended up adding more overall features to our data
  # But, these are the right features. The previous 29 did not have the best ones. 
  juice() 


# * Setting up Tidymodel workflows ----


# Forecasting Workflow leverages tidymodels modeling tools
# linear_reg() initializes a linear regression model specification
# It comes from the parsnip package
model_02_ts_lr <- linear_reg() %>%
  set_mode("regression") %>%
  # We set engine to "lm" since that's what stands for Linear Regression
  set_engine("lm")


# Here we are creating our Tidymodel workflow 
# This will incorporate our model and our recipe in one fell swoop
# workflow() combines both a model and a preprocessing recipe
model_02_ts_lr_workflow <- workflow() %>%
  # add_model() adds a machine learning specification to a workflow
  # Comes from the workflows package
  # We are going to use model_02_ts_lr, which is the linear regression model we initialized above
  add_model(model_02_ts_lr) %>%
  # add_recipe() adds a preprocessing specification (recipe) to a workflow
  # We will use ts_recipe_2 that we created above
  # Remember that we added four steps to our recipe
  add_recipe(ts_recipe_2) %>%
  # Calling fit() on a workflow uses workflows::fit.workflow()
  # Just provide the data that you want to fit
  # Here we are going to fit our training set
  # So we have officially fitted our linear regression model to our training set as is standard practice
  fit(training(train_test_splits))


# Here we are taking a look at our Workflow we just implemented for linear regression
# == Workflow [trained] 
#   Preprocessor: Recipe
# Model: linear_reg()
# 
# -- Preprocessor 
#   4 Recipe Steps
# 
# * step_timeseries_signature()
# * step_rm()
# * step_normalize()
# * step_dummy()
# 
# -- Model 
#   
#   Call:
#   stats::lm(formula = ..y ~ ., data = data)
#   Coefficients: (lots of them so I won't paste them)
model_02_ts_lr_workflow


# * Comparing both time series forecasts  ----


# So here we are combining both the prophet model and linear regression model so we can compare their performance adequately
combined_calibration_tbl <- modeltime_table(
  model_01_ts_prophet,
  model_02_ts_lr_workflow) %>%
  # The fitted models have already been trained on the training set
  # Now we are incorporating (calibrating) the models on the test set
  modeltime_calibrate(testing(train_test_splits))


# Here we are comparing the accuracy between our two models
# It looks like the linear regression model is performing better than the prophet model
# Remember we want lower MAE, RMSE, and higher R-Squared
combined_model_metrics <- combined_calibration_tbl %>%
  modeltime_accuracy()


# * Visualizing both forecasts  ----


# Here we will view the forecasted data from both our models and append them to the real data
combined_forecast_data <- combined_calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(train_test_splits),
    # Remember we want to compare the forecast to the actual data
    # The actual data is our mailchimp_optins_daily_evaluated
    # We get a 581 x 7 tibble
    # .model_desc has "ACTUAL", "PROPHET" and "LM"
    actual_data = mailchimp_optins_daily_evaluated) 


# Here we will plot the two different forecasts against each other
combined_forecast_plot <- combined_forecast_data %>%
  plot_modeltime_forecast()


# So now we have a nice plot that shows the real data and the forecasts from our two models
# There are some spikes in the actual data that our forecasts cannot capture
combined_forecast_plot




# 8.0 Summary & Next Steps ----


# We created two time series forecasts
# One was a Prophet model, the other was a basic linear regression model


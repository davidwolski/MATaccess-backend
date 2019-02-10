library(tidyverse)
library(lubridate)
library(prophet)
options(stringsAsFactors = FALSE)


# Load data ---------------------------------------------------------------

load("output/merged-data.RData")



# Select states to include in the model -----------------------------------

# Either use all states
# include_states <- state.abb %>% 
#   .[. != c("AK", "HI")]

# Or subset to a few states
include_states <- c("MA", "OH", "FL", "CA")


# Subset data for simple use case -----------------------------------------

# Set model type ("full", "validation", or "debug")
mtype <- "full" 

# For now we'll filter the data to one state only. In order to
# predict 2018 data using additional regressors we lag the data by one year,
# i.e. use 2010 data to predict 2011 mortality and so on. This also means that
# we won't get a prediction for 2010, which is something to keep in mind. We do
# this by creating two new variables called target_year and
# target_year_death_rate which we feed into the prophet model as reuquired input
# variables y and ds. Finally we calculate the number of missing mortality rate
# data points per county and filter data to counties that have at least 3 data
# points.

merged_subset <- merged %>% 
  # Filter to state subset
  filter(state_postal %in% include_states) %>%
  # Group by county (FIPS code)
  group_by(fips_county_code) %>%
  # Create target variables with 1-year lead
  mutate(target_year_death_rate = lead(death_rate, order_by = year),
         target_year = year + 1) %>% 
  # Mutate target variables to fit prophet requirements
  mutate(ds = as.Date(as.character(target_year), "%Y"),
         y = target_year_death_rate) %>% 
  # Calculate number of missing data mortality points
  mutate(na_num = sum(is.na(death_rate))) %>%
  # Filter to counties with at least three data points (maximum NA is 8) 
  ungroup() %>% 
  filter(na_num <= 5) 


# Run prophet model -------------------------------------------------------

# Set dataframe for prophet dependent on model type
if (mtype == "full") {
  df <- filter(merged_subset, year < 2018)
} else if (mtype == "validation") {
  df <- filter(merged_subset, year < 2017)
} else {
  df <- merged_subset
}

# Then run the appropriate model
if (mtype == "full" | mtype == "validation") {
  # Run prophet forecast by county. Since we're dealing with annual data, we can 
  # disable weekly and daily seasonality and reduce the verbosity of the code.
  system.time(
    d1 <- df %>%
      # Nest data by FIPS county code
      nest(-fips_county_code) %>% 
      # Create prophet model object
      mutate(m = map(rep(list(NULL), length(unique(.$fips_county_code))), 
                     prophet, 
                     weekly.seasonality = FALSE,
                     daily.seasonality = FALSE)) %>% 
      # Add additional regressors
      mutate(m = map(m, add_regressor, 'prescribing_rate')) %>%
      mutate(m = map(m, add_regressor, 'unemployed_perc')) %>% 
      mutate(m = map(m, add_regressor, 'education_perc')) %>% 
      mutate(m = map(m, add_regressor, 'poverty_perc')) %>%
      mutate(m = map(m, add_regressor, 'income_median')) %>% 
      mutate(m = map(m, add_regressor, 'income_median')) %>% 
      mutate(m = map(m, add_regressor, 'age_median')) %>% 
      mutate(m = map(m, add_regressor, 'sex_male_perc')) %>% 
      # Fit prophet model
      mutate(m = map2(m, data, fit.prophet)) %>%
      # Create future dataframe
      mutate(future = map(m, make_future_dataframe, period = 1, freq = 'year')) %>% 
      mutate(future = nest(df, -fips_county_code)$data) %>% 
      # Predict 
      mutate(forecast = map2(m, future, predict))
  )
  
  # Unnest forecast data to get betahats. Note that we pull out the year in form
  # of the target year for prediction (ds), which is important for merging the
  # output with the input data.
  
  frcst <- d1 %>% 
    unnest(forecast) %>% 
    transmute(year = year(ds), fips_county_code, 
              yhat_lower, yhat_upper, yhat) %>% 
    arrange(fips_county_code, year)
  
  # Get 2010 back into the data frame by building an index and splitting it per
  # county, at which point we can add a row to each group. This is necessary
  # since we're lagging the data and are not making predictions for 2010.
  
  # First we build indices based on the frcst data frame
  indices <- seq(nrow(frcst)) %>% 
    split(group_indices(frcst, fips_county_code)) %>% 
    map(~c(NA, .x)) %>%
    unlist
  
  # Then we create a new data frame
  frcst2 <- frcst[indices,] %>% 
    group_by(fips_county_code) %>% 
    mutate(year = ifelse(is.na(year), 2010, year)) %>% 
    ungroup() %>% 
    mutate(fips_county_code = ifelse(is.na(fips_county_code), 
                                     lead(fips_county_code),
                                     fips_county_code)) %>% 
    # A few forecasts produce negative values, which isn't a real-world
    # possibility, so we'll set those predicted values to 0 by moving both the
    # predicted value and it's lower and upper bound up by it's predicted value
    mutate(yhat_lower = ifelse(yhat < 0, yhat_lower-yhat, yhat_lower),
           yhat_upper = ifelse(yhat < 0, yhat_upper-yhat, yhat_upper),
           yhat = ifelse(yhat < 0, yhat-yhat, yhat))
  
  
  # Now we can merge our forecasting results with the input data frame. Note that
  # we are merging the data on year and fips code, so mortality predictions align
  # with the years on the input data, not with the data used to create them.
  merged_res <- merged %>% 
    # Right join with frcst2 data, to restrict results to counties in model
    right_join(frcst2) %>% 
    # Group by county and year
    group_by(fips_county_code) %>%
    arrange(year) %>% 
    # Create variables that provide mortality rate increases over the previous 
    # year for both predicted and actual rates. Increases for predicted rates are
    # calculated as rate increase of predicted vs previous year actual.
    mutate(state_full = state_full[1],
           county_mortality = county_mortality[1],
           state_postal = state_postal[1],
           mortality_increase_actual = (death_rate/lag(death_rate)*100)-100,
           mortality_increase_predicted = (yhat/lag(death_rate)*100)-100)
}

# Full model output -------------------------------------------------------

# If we're working with the full model, we can simply select the columns of 
# interest from merged_res and save them

if (mtype == "full") {
  # Select and rename columns
  merged_res_select <- merged_res %>% 
    transmute(
      "Year" = year,
      "State" = state_full,
      "State Abbreviated" = state_postal,
      "County" = county_mortality,
      "FIPS Code" = fips_county_code,
      "Population" = population,
      "Mortality Count" = deaths,
      "Mortality Rate" = round(death_rate,2),
      "Prescriptions" = prescriptions,
      "Prescribing Rate" = prescribing_rate,
      "High School Degree or higher %" = education_perc,
      "Unemployment %" = unemployed_perc,
      "Median Income" = income_median,
      "Poverty %" = poverty_perc,
      "Sex Male %" = sex_male_perc,
      "Median Age" = age_median,
      "Ethnic Majority" = ethnic_majority,
      "Predicted Mortality Rate" = round(yhat,2),
      "Prediction Lower Bound" = round(yhat_lower,2),
      "Prediction Upper Bound" = round(yhat_upper,2),
      "Predicted Mortality Increase" = round(mortality_increase_predicted,1),
      "Actual Mortality Increase" = round(mortality_increase_actual,1)
    )
  
  save(merged_res_select, file = "output/merged-results.RData")
}


# Validation model output -------------------------------------------------

# If we're working with the reduced model for validation, we would like a 
# different output
if (mtype == "validation") {
  
  # First, we calculate some summary stats for the validation model. We filter
  # the merged results data frame to exclude 2010 and then calculate mean
  # squared error (MSE), average MSE over all counties and percentile of average
  # MSE. Because some years are missing for some counties, the average MSE won't
  # be the same for all groupings, which is why we get the max value for the MSE
  # after ungrouping the data. This could be written more elegantly with
  # summarize functions, but this approach works fine for our approach.
  
  # First we define a function to get the MSE
  mse_fun <- function(y_hat, y) {mean((y_hat-y)^2, na.rm = TRUE)}
  
  # Then we create the data frame
  validation <- merged_res %>%
    # Filter to the prediction year
    filter(year == "2017") %>%
    # Calculate MSE
    group_by(fips_county_code) %>%
    dplyr::mutate(MSE = mse_fun(yhat, death_rate)) %>%
    ungroup() %>%
    # Correct average MSE and create MSE percentile
    mutate(averageMSE = mean(MSE, na.rm = TRUE),
           MSEpercentile = (ecdf(unique(MSE))(MSE))*100)
  
  # Select summary stats columns
  validation_select <- validation %>%
    select(fips_county_code, MSE, averageMSE, MSEpercentile, year)
  
  
  # Next, we build a simple linear model that we use to fit to each county data
  # to compare to our more complex prophet model
  
  linmod_fun <- function(df) {
    lm(y ~ ds + prescribing_rate, data = df)
  }
  
  mse_fun <- function(x) {
    mean(x$residuals^2)
  }
  
  # Create linear models
  linmods <- df %>%
    nest(-fips_county_code) %>%
    mutate(linmod = map(data, linmod_fun)) %>%
    mutate(summod = map(linmod, summary)) %>%
    mutate("LM MSE" = map(summod, mse_fun))
  
  # Unnest MSE data from linear model
  validation_linmod_select <- linmods %>%
    unnest(`LM MSE`) %>%
    select(fips_county_code, `LM MSE`) %>%
    right_join(validation_select) %>%
    group_by(year) %>%
    mutate(`LM averageMSE` = round(mean(`LM MSE`),4)) %>%
    # Correct average MSE and create MSE percentile
    ungroup() %>%
    mutate(`LM averageMSE` = max(`LM averageMSE`),
           `LM MSEpercentile` = round((ecdf(unique(`LM MSE`))(`LM MSE`))*100, 0)) %>%
    mutate(`MSE improvement` = (averageMSE - `LM averageMSE`)/averageMSE *100) %>% 
    select(-year) %>% 
    distinct() %>% 
    # Do some additional rounding of variables to improve output legibility
    mutate(MSEpercentile = round(MSEpercentile, 0),
           averageMSE = round(averageMSE, 4))
  
  
  save(validation_linmod_select, file = "output/model-validation.RData")
}



# Model a single county ---------------------------------------------------

if (mtype == "debug") {
  # First, filter input data
  df <- filter(merged_subset, year < 2018 & county_mortality == "Suffolk County")
  
  # Fit simple linear model
  linmod <- lm(y ~ ds + prescribing_rate, data = df)
  summod <- summary(linmod)
  msemod <- mean(summod$residuals^2)
  
  # Fit prophet model
  m <- prophet() %>% 
    add_regressor('prescribing_rate') %>% 
    add_regressor('education_perc') %>% 
    add_regressor('unemployed_perc') %>% 
    add_regressor('income_median') %>% 
    add_regressor('poverty_perc') %>% 
    add_regressor('sex_male_perc') %>% 
    add_regressor('age_median') %>% 
    fit.prophet(m, df)
  
  # Perform prophet cross-validation
  # df.cv <- cross_validation(m, horizon = 52, units = "weeks", period = 52,
  #                                initial = 2*(52))
  # 
  # df.p <- performance_metrics(df.cv)
  # plot_cross_validation_metric(df.cv, metric = 'mape')
  
  # Create future data frame for prophet, by either supplying the original input
  # data frame or by using the corresponding function in prophet
  future <- filter(df, county_mortality == "Suffolk County")
  # future <- make_future_dataframe(m, periods = 1, freq = 'year')
  
  # Run prediction
  forecast <- predict(m, future)
  
  merged <- forecast %>%
    mutate(year = year(ds)) %>% 
    add_row(year = 2010) %>% 
    right_join(df, by = "year") 
  
  merged_stats <- merged %>%
    filter(year > 2010) %>% 
    group_by(fips_county_code) %>% 
    mutate(MSE = mean((yhat-death_rate)^2))
  
  
  # Plot
  plot(m,forecast)
  prophet_plot_components(m, forecast)
  dyplot.prophet(m, forecast)
}
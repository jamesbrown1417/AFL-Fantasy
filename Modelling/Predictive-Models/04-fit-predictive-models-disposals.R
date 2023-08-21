##%######################################################%##
#                                                          #
####           Fit models to player disposals           ####
#                                                          #
##%######################################################%##

#===============================================================================
# Libraries
#===============================================================================

library(brms)
library(rstanarm)
library(tidyverse)
library(mongolite)
library(glmnet)
library(splines)

#===============================================================================
# Read in Training, Target and Test Data
#===============================================================================

training <- read_rds("Modelling/Predictive-Models/Data/training_data_disposals.rds")
test <- read_rds("Modelling/Predictive-Models/Data/test_data_disposals.rds")

#===============================================================================
# Filter to only season for faster training (comment out if want to use full data)
#===============================================================================

training <- training |> filter(season == 2023)

#===============================================================================
# Standardise predictors
#===============================================================================

training_std <-
  training |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(position = factor(position)) |> 
  mutate(disposals = as.integer(disposals), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

test_std <-
  test |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(position = factor(position)) |> 
  mutate(disposals = as.integer(disposals), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

#===============================================================================
# Create spline terms for margin
#===============================================================================

training$margin_spline <- bs(training$margin, df = 5)
test$margin_spline <- bs(test$margin, df = 5)

training_std$margin_spline <- bs(training_std$margin, df = 5)
test_std$margin_spline <- bs(test_std$margin, df = 5)

##%######################################################%##
#                                                          #
####            Fit a regular Poisson model             ####
#                                                          #
##%######################################################%##

#===============================================================================
# Fit the model
#===============================================================================

mod_1 <-
  glm(
    disposals ~
      home_game +
      margin_spline +

      dvp_10 +
      percentage_over_avg_10 +
      
      last_3_avg*last_5_avg*last_10_avg +
      last_5_min*last_10_min +
      last_5_max*last_10_max +
      last_5_variance*last_10_variance,
      
    data = training_std,
    family = poisson(link = "log")
  )

summary(mod_1)

#===============================================================================
# Get predictions on the training data
#===============================================================================

# Predictions col
predictions <- predict(mod_1, newdata = test_std, type = "response")

# Bind to test data
test_predictions_1 <- test
test_predictions_1$predicted_disposals = predictions

# Get squared error
test_predictions_1$squared_error <- (test_predictions_1$disposals - test_predictions_1$predicted_disposals)^2
test_predictions_1$absolute_error <- abs(test_predictions_1$disposals - test_predictions_1$predicted_disposals)

# Select variables
test_predictions_1 <-
  test_predictions_1 |>
  select(
    player_full_name,
    player_team,
    opposition_team,
    margin,
    round,
    season,
    home_game,
    disposals,
    predicted_disposals,
    squared_error,
    absolute_error
  )

# Get MSE
MSE_1 <- mean(test_predictions_1$squared_error)
MAE_1 <- mean(test_predictions_1$absolute_error)

##%######################################################%##
#                                                          #
####            Fit a Bayesian Poisson Model            ####
#                                                          #
##%######################################################%##

#===============================================================================
# Fit the model
#===============================================================================

mod_2 <-
  stan_glm(
    disposals ~
      home_game +
      margin_spline +
      dvp_10 +
      percentage_over_avg_10 +
      last_3_avg*last_5_avg*last_10_avg +
      last_5_min*last_10_min +
      last_5_max*last_10_max +
      last_5_variance*last_10_variance,
    family = poisson(link = "log"),
    data = training,
    control = list(max_treedepth = 10),
    iter = 2000,
    warmup = 1000,
    cores = 12,
    chains = 6,
    prior = normal(0, 0.1, autoscale = TRUE))

# Check posterior predictions vs observed data
pp_check(mod_2)

##%######################################################%##
#                                                          #
####      Fit a Bayesian Negative Binomial Model        ####
#                                                          #
##%######################################################%##

#===============================================================================
# Fit the model
#===============================================================================

# Update to neg binomial 2 model
mod_3 <- update(mod_2, family = neg_binomial_2, prior_aux = exponential(rate = 10, autoscale = TRUE)) 

# Check posterior predictions vs observed data
pp_check(mod_3)

##%######################################################%##
#                                                          #
####         Fit Best Model (Negative Binomial          ####
####           at this stage) using all data            ####
#                                                          #
##%######################################################%##



##%######################################################%##
#                                                          #
####               Save model objects for               ####
####           use in generating predictions            ####
#                                                          #
##%######################################################%##

# Poisson Model
save(mod_2, file = "Modelling/Predictive-Models/fitted_models/disposals_stan_poisson.RData")

# Negative Binomial 2 Model - Testing
save(mod_3, file = "Modelling/Predictive-Models/fitted_models/disposals_stan_neg_binomial2.RData")

# Negative Binomial 2 Model - This Round's Prediction
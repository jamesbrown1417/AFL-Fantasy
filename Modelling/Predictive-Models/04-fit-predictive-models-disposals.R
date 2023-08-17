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

#===============================================================================
# Read in Training, Target and Test Data
#===============================================================================

training <- read_rds("Modelling/Predictive-Models/Data/training_data_disposals.rds")
test <- read_rds("Modelling/Predictive-Models/Data/test_data_disposals.rds")
target <- read_rds("Modelling/Predictive-Models/Data/target_data_disposals.rds")

#===============================================================================
# Add lines data as predicted margin
#===============================================================================

uri <- Sys.getenv("mongodb_connection_string")

lines_con <- mongo(collection = "Lines", db = "Odds", url = uri)

lines <- lines_con$find('{}') |> tibble()

home_lines_data <-
  lines |>
  separate(match, into = c("home_team", "away_team"), sep = " v ") |>
  select(player_team = home_team, margin = home_line)

away_lines_data <-
  lines |>
  separate(match, into = c("home_team", "away_team"), sep = " v ") |>
  select(player_team = away_team, margin = away_line)

margin_data <-
  bind_rows(home_lines_data, away_lines_data) |>
  mutate(player_team = ifelse(player_team == "West Coast", "West Coast Eagles", player_team)) |>
  mutate(player_team = ifelse(player_team == "Adelaide", "Adelaide Crows", player_team)) |>
  mutate(player_team = ifelse(player_team == "Geelong", "Geelong Cats", player_team)) |>
  mutate(player_team = ifelse(player_team == "Gold Coast", "Gold Coast Suns", player_team)) |>
  mutate(player_team = ifelse(player_team == "Greater Western Sydney", "GWS Giants", player_team)) |>
  mutate(player_team = ifelse(player_team == "Sydney", "Sydney Swans", player_team)) |>
  mutate(margin = -1 * margin)

target <-
  target |>
  left_join(margin_data)

target <- target[complete.cases(target), ]

#===============================================================================
# Standardise predictors
#===============================================================================

training_std <-
  training |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(disposals = as.integer(disposals), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

test_std <-
  test |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(disposals = as.integer(disposals), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

target_std <-
  target |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

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
      margin +
      dvp_5 +
      dvp_10 +
      dvp_15 +
      
      percentage_over_avg_5 +
      percentage_over_avg_10 +
      percentage_over_avg_15 +
      
      last_3_avg +
      last_3_min +
      last_3_max +
      last_3_variance +
      
      last_5_avg +
      last_5_min +
      last_5_max +
      last_5_variance +
      
      last_7_avg +
      last_7_min +
      last_7_max +
      last_7_variance +
      
      last_10_avg +
      last_10_min +
      last_10_max +
      last_10_variance,
    data = training_std,
    family = poisson(link = "log")
  )

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
      margin +
      dvp_5 +
      dvp_10 +
      dvp_15 +
      
      percentage_over_avg_5 +
      percentage_over_avg_10 +
      percentage_over_avg_15 +
      
      last_3_avg +
      last_3_min +
      last_3_max +
      last_3_variance +
      
      last_5_avg +
      last_5_min +
      last_5_max +
      last_5_variance +
      
      last_7_avg +
      last_7_min +
      last_7_max +
      last_7_variance +
      
      last_10_avg +
      last_10_min +
      last_10_max +
      last_10_variance,
    family = poisson(link = "log"),
    data = training,
    control = list(max_treedepth = 15),
    iter = 5000,
    warmup = 2500,
    cores = 12,
    prior = normal(0,1, autoscale = TRUE))

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
mod_3 <- update(mod_2, family = neg_binomial_2) 

# Check posterior predictions vs observed data
pp_check(mod_3)

##%######################################################%##
#                                                          #
####               Save model objects for               ####
####           use in generating predictions            ####
#                                                          #
##%######################################################%##

# Poisson Model
save(mod_2, file = "Modelling/Predictive-Models/fitted_models/disposals_stan_poisson.RData")

# Negative Binomial 2 Model
save(mod_3, file = "Modelling/Predictive-Models/fitted_models/disposals_stan_neg_binomial2.RData")

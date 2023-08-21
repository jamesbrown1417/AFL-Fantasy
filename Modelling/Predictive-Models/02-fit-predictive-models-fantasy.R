##%######################################################%##
#                                                          #
####        Fit models to player fantasy scores         ####
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

options(mc.cores = parallel::detectCores())

#===============================================================================
# Read in Training, Target and Test Data
#===============================================================================

training <- read_rds("Modelling/Predictive-Models/Data/training_data_fantasy.rds")
test <- read_rds("Modelling/Predictive-Models/Data/test_data_fantasy.rds")
target <- read_rds("Modelling/Predictive-Models/Data/target_data_fantasy.rds")

# Current round training data
current_round_training_data <-
  bind_rows(training, test) |>
  arrange(player_full_name, player_team, round)


# Remove duplicate players------------------------------------------------------

# Get 2023 players and teams
player_teams_2023 <-
  training |>
  filter(season == 2023) |>
  transmute(id = paste(player_full_name, player_team))

# Filter
target <-
target |>
  filter(paste(player_full_name, player_team) %in% player_teams_2023$id)

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
  mutate(fantasy_points = as.integer(fantasy_points), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

test_std <-
  test |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(fantasy_points = as.integer(fantasy_points), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

target_std <-
  target |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

#===============================================================================
# Create spline terms for margin
#===============================================================================

training$margin_spline <- bs(training$margin, df = 5)
test$margin_spline <- bs(test$margin, df = 5)
target$margin_spline <- bs(target$margin, df = 5)
current_round_training_data$margin_spline <- bs(current_round_training_data$margin, df = 5)

##%######################################################%##
#                                                          #
####                 Fit linear model                   ####
#                                                          #
##%######################################################%##

#===============================================================================
# Fit the model
#===============================================================================

mod_1 <-
  glm(
    fantasy_points ~
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
    data = training_std
  )

# Try LASSO---------------------------------------------------------------------

X <-
  model.matrix(
  fantasy_points ~
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
  data = training_std
) 

X_test <-
  model.matrix(
  fantasy_points ~
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
  data = test_std
) 

X <- X[,-1]

X_test <- X_test[,-1]

Y <- training_std$fantasy_points

set.seed(123) # for reproducibility
cv.fit <- cv.glmnet(X, Y, alpha = 1) # alpha = 1 indicates Lasso regression

best_lambda <- cv.fit$lambda.min
mod_2 <- glmnet(X, Y, alpha = 1, lambda = best_lambda)

#===============================================================================
# Get predictions on the training data
#===============================================================================

#===============================================================================
# Linear Model
#===============================================================================

# Predictions col
predictions <- predict(mod_1, newdata = test_std)

# Bind to test data
test_predictions_1 <- test
test_predictions_1$predicted_fantasy = predictions

# Get squared error
test_predictions_1$squared_error <- (test_predictions_1$fantasy_points - test_predictions_1$predicted_fantasy)^2
test_predictions_1$absolute_error <- abs(test_predictions_1$fantasy_points - test_predictions_1$predicted_fantasy)

# Get MSE
MSE_1 <- mean(test_predictions_1$squared_error)
MAE_1 <- mean(test_predictions_1$absolute_error)

#===============================================================================
# Lasso
#===============================================================================

# Predictions col
predictions <- predict(mod_2, newx = X_test)

# Bind to test data
test_predictions_2 <- test
test_predictions_2$predicted_fantasy = predictions

# Get squared error
test_predictions_2$squared_error <- (test_predictions_2$fantasy_points - test_predictions_2$predicted_fantasy)^2
test_predictions_2$absolute_error <- abs(test_predictions_2$fantasy_points - test_predictions_2$predicted_fantasy)

# Get MSE
MSE_2 <- mean(test_predictions_2$squared_error)
MAE_2 <- mean(test_predictions_2$absolute_error)

##%######################################################%##
#                                                          #
####             Fit Bayesian Linear Model              ####
#                                                          #
##%######################################################%##

# Calculate log of the mean of fantasy_points for an intercept prior
mean(training$fantasy_points)

# Fit the bayesian linear model using RStanarm
mod_3 <-
  stan_glm(
    fantasy_points ~
      home_game +
      margin_spline +
      dvp_10 +
      percentage_over_avg_10 +
      last_3_avg*last_5_avg*last_10_avg +
      last_5_min*last_10_min +
      last_5_max*last_10_max +
      last_5_variance*last_10_variance,
    family = gaussian(link = "identity"),
    data = training,
  control = list(max_treedepth = 15),
  iter = 5000,
  warmup = 2500,
  prior = normal(0,0.1,autoscale = TRUE),
  prior_aux = exponential(rate = 10, autoscale = TRUE))

# Generate Samples
pp_samples_3 <- posterior_predict(mod_3, newdata = test, ndraws = 10000)

# Get means
pp_means_3 <- apply(pp_samples_3, 2, mean)

# Add to test
test_predictions_3 <- test
test_predictions_3$predicted_fantasy = pp_means_3

# Get squared error
test_predictions_3$squared_error <- (test_predictions_3$fantasy_points - test_predictions_3$predicted_fantasy)^2
test_predictions_3$absolute_error <- abs(test_predictions_3$fantasy_points - test_predictions_3$predicted_fantasy)

# Get MSE
MSE_3 <- mean(test_predictions_3$squared_error)
MAE_3 <- mean(test_predictions_3$absolute_error)

# Get lines
pp_median_3 <- apply(pp_samples_3, 2, median)

# Lines-------------------------------------------------------------------------

# Function to get line
round_to_nearest_half <- function(x) {
  floor_value <- floor(x)
  lower_bound = floor_value + 0.5
  upper_bound = floor_value + 1.5
  
  if (abs(x - lower_bound) <= abs(x - upper_bound)) {
    return(lower_bound)
  } else {
    return(upper_bound)
  }
}

pp_median_3 <- sapply(pp_median_3, round_to_nearest_half)

# Create lines
fantasy_lines <-
test |>
  dplyr::select(player_full_name, player_team, opposition_team, round, fantasy_points) |>
  bind_cols("predicted_fantasy_line" = pp_median_3) |>
  mutate(covered_line = fantasy_points > predicted_fantasy_line)

sum(fantasy_lines$covered_line) / nrow(fantasy_lines)

##%######################################################%##
#                                                          #
####         Fit the model for this weeks data          ####
#                                                          #
##%######################################################%##

# Fit model to current round training data
mod_4 <-
  stan_glm(
    fantasy_points ~
      home_game +
      margin_spline +
      dvp_10 +
      percentage_over_avg_10 +
      last_3_avg*last_5_avg*last_10_avg +
      last_5_min*last_10_min +
      last_5_max*last_10_max +
      last_5_variance*last_10_variance,
    family = gaussian(link = "identity"),
    data = current_round_training_data,
    control = list(max_treedepth = 15),
    iter = 5000,
    warmup = 2500,
    prior = normal(0,0.1,autoscale = TRUE),
    prior_aux = exponential(rate = 100, autoscale = TRUE))

# Generate Samples
pp_samples_4 <- posterior_predict(mod_4, newdata = target, ndraws = 10000)

# Get means
pp_means_4 <- apply(pp_samples_4, 2, mean)

# Get lines
pp_median_4 <- apply(pp_samples_4, 2, median)

current_round_lines <- sapply(pp_median_4, round_to_nearest_half)

# Create lines
fantasy_lines_upcoming_round <-
  target |>
  dplyr::select(player_full_name, player_team, opposition_team, round) |>
  bind_cols("predicted_fantasy_line" = current_round_lines)

# Add to upcoming match lines
fantasy_lines_upcoming_round$prob_70_plus <- apply(pp_samples_4, 2, function(x) mean(x >= 70))
fantasy_lines_upcoming_round$prob_80_plus <- apply(pp_samples_4, 2, function(x) mean(x >= 80))
fantasy_lines_upcoming_round$prob_90_plus <- apply(pp_samples_4, 2, function(x) mean(x >= 90))
fantasy_lines_upcoming_round$prob_100_plus <- apply(pp_samples_4, 2, function(x) mean(x >= 100))
fantasy_lines_upcoming_round$prob_110_plus <- apply(pp_samples_4, 2, function(x) mean(x >= 110))
fantasy_lines_upcoming_round$prob_120_plus <- apply(pp_samples_4, 2, function(x) mean(x >= 120))

#===============================================================================
# Compare with actual odds to find biggest differences
#===============================================================================

# Read in datasets--------------------------------------------------------------
uri <- Sys.getenv("mongodb_connection_string")

fantasy_con <- mongo(collection = "Fantasy", db = "Odds", url = uri)

fantasy <- fantasy_con$find('{}') |> tibble() |> distinct(player_name, fantasy_points, agency, .keep_all = TRUE)

# Compare with predictions------------------------------------------------------
pred_70 <-
fantasy_lines_upcoming_round |>
  transmute(player_name = player_full_name,
            fantasy_points = 69.5,
            over_predicted_probability = prob_70_plus)

pred_80 <-
  fantasy_lines_upcoming_round |>
  transmute(player_name = player_full_name,
            fantasy_points = 79.5,
            over_predicted_probability = prob_80_plus)
pred_90 <-
  fantasy_lines_upcoming_round |>
  transmute(player_name = player_full_name,
            fantasy_points = 89.5,
            over_predicted_probability = prob_90_plus)
pred_100 <-
  fantasy_lines_upcoming_round |>
  transmute(player_name = player_full_name,
            fantasy_points = 99.5,
            over_predicted_probability = prob_100_plus)
pred_110 <-
  fantasy_lines_upcoming_round |>
  transmute(player_name = player_full_name,
            fantasy_points = 109.5,
            over_predicted_probability = prob_110_plus)

pred_120 <-
  fantasy_lines_upcoming_round |>
  transmute(player_name = player_full_name,
            fantasy_points = 119.5,
            over_predicted_probability = prob_120_plus)

predicted_lines <- bind_rows(pred_70, pred_80, pred_90, pred_100, pred_110, pred_120)

# Get comparisons
fantasy_comparisons_neds <-
fantasy |>
  filter(agency %in% c("Neds")) |>
  left_join(predicted_lines) |>
  select(match, agency, player_name, fantasy_points, over_price, over_implied_probability, over_predicted_probability) |>
  mutate(diff = over_predicted_probability - over_implied_probability) |>
  arrange(desc(diff))

fantasy_comparisons_pb <-
  fantasy |>
  filter(agency %in% c("Pointsbet")) |>
  left_join(predicted_lines) |>
  select(match, agency, player_name, fantasy_points, over_price, over_implied_probability, over_predicted_probability) |>
  mutate(diff = over_predicted_probability - over_implied_probability) |>
  arrange(desc(diff))

fantasy_comparisons_betright <-
  fantasy |>
  filter(agency %in% c("Betright")) |>
  left_join(predicted_lines) |>
  select(match, agency, player_name, fantasy_points, over_price, over_implied_probability, over_predicted_probability) |>
  mutate(diff = over_predicted_probability - over_implied_probability) |>
  arrange(desc(diff))

fantasy_comparisons_topsport <-
  fantasy |>
  filter(agency %in% c("Topsport")) |>
  left_join(predicted_lines) |>
  select(match, agency, player_name, fantasy_points, over_price, over_implied_probability, over_predicted_probability) |>
  mutate(diff = over_predicted_probability - over_implied_probability) |>
  arrange(desc(diff))

#===============================================================================
# Output upcoming weeks to model testing to assess performance
#===============================================================================

# fantasy_comparisons_neds |> 
#   write_rds("Modelling/Predictive-Models/model_testing/neds_fantasy_round_23_2023.rds")

# fantasy_comparisons_pb |>
#   write_rds("Modelling/Predictive-Models/model_testing/pointsbet_fantasy_round_23_2023.rds")


##%######################################################%##
#                                                          #
####         Fit Poisson and negative binomial          ####
####          models to model player Fantasy            ####
#                                                          #
##%######################################################%##

#===============================================================================
# Libraries
#===============================================================================

library(MASS)
library(rms)
library(brms)
library(tidyverse)
library(mongolite)

#===============================================================================
# Read in Training, Target and Test Data
#===============================================================================

training <- read_rds("Modelling/Predictive-Models/training_data.rds")
test <- read_rds("Modelling/Predictive-Models/test_data.rds")
target <- read_rds("Modelling/Predictive-Models/target_data.rds")

#===============================================================================
# Prepare data for model fitting
#===============================================================================

# Arrange in order
training <-
training |>
  arrange(player_full_name, round)

# Create min, max, and variance for last 3, 5, and 7 games predictors
training <-
  training |> 
  rowwise() |> 
  mutate(
    last_7_min = min(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7)),
    last_7_max = max(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7)),
    last_7_var = var(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7), na.rm = TRUE)
  ) |> 
  ungroup()


test <-
  test |>
  rowwise() |> 
  mutate(
    last_7_min = min(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7)),
    last_7_max = max(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7)),
    last_7_var = var(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7), na.rm = TRUE)
  ) |> 
  ungroup()

#===============================================================================
# Add last 7 min max and var variables to target
#===============================================================================

target <-
  bind_rows(training, test, target) |>
  arrange(player_full_name, round) |>
  group_by(player_full_name, player_team) |>
  mutate(
    fantasy_lag_1 = lag(fantasy_points),
    fantasy_lag_2 = lag(fantasy_lag_1),
    fantasy_lag_3 = lag(fantasy_lag_2),
    fantasy_lag_4 = lag(fantasy_lag_3),
    fantasy_lag_5 = lag(fantasy_lag_4),
    fantasy_lag_6 = lag(fantasy_lag_5),
    fantasy_lag_7 = lag(fantasy_lag_6)
  ) |>
  rowwise() |> 
  mutate(
    last_7_min = min(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7), na.rm = TRUE),
    last_7_max = max(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7), na.rm = TRUE),
    last_7_var = var(c(fantasy_lag_1, fantasy_lag_2, fantasy_lag_3, fantasy_lag_4, fantasy_lag_5, fantasy_lag_6, fantasy_lag_7), na.rm = TRUE)
  ) |> 
  ungroup() |>
  filter(round == max(round, na.rm = TRUE))
  
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
  select(player_full_name, player_team, opposition_team, round, home_game:last_7_var) |>
  left_join(margin_data)

target <- target[complete.cases(target), ]

#===============================================================================
# Standardise predictors
#===============================================================================

training <-
training |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(fantasy_points = as.integer(fantasy_points), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

test <-
  test |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(fantasy_points = as.integer(fantasy_points), margin = as.double(margin)) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

target <-
  target |>
  mutate(home_game = factor(home_game, labels = c("No", "Yes"))) |>
  mutate(across(where(is.double), ~ as.numeric(scale(.x))))

##%######################################################%##
#                                                          #
####                 Fit poisson model                  ####
#                                                          #
##%######################################################%##

#===============================================================================
# Fit the model
#===============================================================================

mod_1 <-
  glm(
    fantasy_points ~
      home_game +
      margin*dvp +
      last_3_avg*last_7_avg +
      last_7_min +
      last_7_max +
      last_7_var +
      season_avg,
    data = training
  )

summary(mod_1)

#===============================================================================
# Get predictions on the training data
#===============================================================================

# Predictions col
predictions <- predict(mod_1, newdata = test)

# Bind to test data
test_predictions_1 <- test
test_predictions_1$predicted_fantasy = predictions

# Get squared error
test_predictions_1$squared_error <- (test_predictions_1$fantasy_points - test_predictions_1$predicted_fantasy)^2

# Get MSE
MSE_1 <- mean(test_predictions_1$squared_error)

##%######################################################%##
#                                                          #
####             Fit Bayesian Linear Model              ####
#                                                          #
##%######################################################%##

# Calculate log of the mean of fantasy_points for an intercept prior
log(mean(training$fantasy_points))

# Fit the bayesian Poisson model
mod_2 <- brm(
  fantasy_points ~
    home_game +
    margin*dvp +
    last_3_avg*last_7_avg +
    last_7_min +
    last_7_max +
    last_7_var +
    season_avg, 
  family = gaussian(link = "identity"),
  data = training,
  control = list(max_treedepth = 15),
  iter = 5000,
  warmup = 2500,
  prior = c(
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "dvp"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "home_gameYes"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_3_avg"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_3_avg:last_7_avg"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_avg"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_max"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_min"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_var"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "margin"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "margin:dvp"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "season_avg"),
    set_prior("student_t(3, 70, 3)", class = "Intercept")
  )
)

# Generate Samples
pp_samples_2 <- posterior_predict(mod_2, newdata = test, ndraws = 10000)

# Get means
pp_means_2 <- apply(pp_samples_2, 2, mean)

# Add to test
test_predictions_2 <- test
test_predictions_2$predicted_fantasy = pp_means_2

# Get squared error
test_predictions_2$squared_error <- (test_predictions_2$fantasy_points - test_predictions_2$predicted_fantasy)^2
test_predictions_2$absolute_error <- abs(test_predictions_2$fantasy_points - test_predictions_2$predicted_fantasy)

# Get probability of scoring over 100
prob_100_or_greater <- apply(pp_samples_2, 2, function(x) mean(x >= 100))

# Get probability of scoring over 110
prob_110_or_greater <- apply(pp_samples_2, 2, function(x) mean(x >= 110))

# Get probability of scoring over 150
prob_150_or_greater <- apply(pp_samples_2, 2, function(x) mean(x >= 150))

# Add to test
test_predictions_2$prob_100_plus <- prob_100_or_greater
test_predictions_2$prob_110_plus <- prob_110_or_greater
test_predictions_2$prob_150_plus <- prob_150_or_greater

# Get MSE
MSE_2 <- mean(test_predictions_2$squared_error)
MAE_2 <- mean(test_predictions_2$absolute_error)

# Get lines
pp_median_2 <- apply(pp_samples_2, 2, median)

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

pp_median_2 <- sapply(pp_median_2, round_to_nearest_half)

# Create lines
fantasy_lines <-
test |>
  dplyr::select(player_full_name, player_team, opposition_team, round, fantasy_points) |>
  bind_cols("predicted_fantasy_line" = pp_median_2) |>
  mutate(covered_line = fantasy_points > predicted_fantasy_line)

sum(fantasy_lines$covered_line) / nrow(fantasy_lines)

##%######################################################%##
#                                                          #
####         Fit the model for this weeks data          ####
#                                                          #
##%######################################################%##

# Current round training data
current_round_training_data <- bind_rows(training, test) |>
  arrange(player_full_name, player_team, round)

# Fit model to current round training data
mod_3 <- brm(
  fantasy_points ~
    home_game +
    margin*dvp +
    last_3_avg*last_7_avg +
    rcs(last_7_min, 3) +
    rcs(last_7_max, 3) +
    rcs(last_7_var, 3) +
    rcs(season_avg, 3), 
  family = gaussian(link = "identity"),
  data = current_round_training_data,
  control = list(max_treedepth = 15),
  iter = 5000,
  warmup = 2500,
  prior = c(
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "dvp"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "home_gameYes"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_3_avg"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_3_avg:last_7_avg"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_avg"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_max"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_min"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "last_7_var"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "margin"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "margin:dvp"),
    set_prior("student_t(5, 0, 0.8)", class = "b", coef = "season_avg"),
    set_prior("student_t(3, 70, 3)", class = "Intercept")
  )
)

# Generate Samples
pp_samples_3 <- posterior_predict(mod_3, newdata = target, ndraws = 10000)

# Get means
pp_means_3 <- apply(pp_samples_3, 2, mean)

# Get lines
pp_median_3 <- apply(pp_samples_3, 2, median)

current_round_lines <- sapply(pp_median_3, round_to_nearest_half)

# Create lines
fantasy_lines_upcoming_round <-
  target |>
  dplyr::select(player_full_name, player_team, opposition_team, round) |>
  bind_cols("predicted_fantasy_line" = current_round_lines)

# Add to upcoming match lines
fantasy_lines_upcoming_round$prob_70_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 70))
fantasy_lines_upcoming_round$prob_80_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 80))
fantasy_lines_upcoming_round$prob_90_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 90))
fantasy_lines_upcoming_round$prob_100_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 100))
fantasy_lines_upcoming_round$prob_110_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 110))
fantasy_lines_upcoming_round$prob_120_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 120))

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
fantasy_comparisons <-
fantasy |>
  filter(agency %in% c("Pointsbet", "Betright", "TopSport")) |>
  left_join(predicted_lines) |>
  select(match, agency, player_name, fantasy_points, over_price, over_implied_probability, over_predicted_probability) |>
  mutate(diff = over_predicted_probability - over_implied_probability) |>
  arrange(desc(diff))

#===============================================================================
# Compare with pointsbet lines
#===============================================================================

# Pointsbet Lines
pointsbet_fantasy_lines <-
fantasy |>
  filter(agency == "Pointsbet") |>
  filter(over_price == 1.87) |>
  select(match, player_name, pointsbet_line = fantasy_points)

# Compare with predicted
pointsbet_vs_predicted_lines <-
fantasy_lines_upcoming_round |>
  select(player_name = player_full_name, predicted_fantasy_line) |>
  right_join(pointsbet_fantasy_lines) |>
  select(match, player_name, pointsbet_line, predicted_fantasy_line) |>
  mutate(diff = predicted_fantasy_line - pointsbet_line) |>
  filter(!is.na(diff))

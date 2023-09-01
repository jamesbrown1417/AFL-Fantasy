##%######################################################%##
#                                                          #
####       Create Brownlow Medal prediction model       ####
#                                                          #
##%######################################################%##

# libraries
library(MASS)
library(tidyverse)
`%notin%` = base::Negate(`%in%`)
library(data.table)
library(rms)

# Read in data
brownlow_analysis_data <-
  read_rds("Modelling/Brownlow/brownlow_analysis_data.rds") |>
  mutate(brownlow_votes = ifelse(is.na(brownlow_votes), 0, brownlow_votes),
         coaches_votes = ifelse(is.na(coaches_votes), 0, coaches_votes)) |>
  mutate(brownlow_votes = factor(brownlow_votes, levels = c(0,1,2,3), ordered = TRUE))

# Split into test and training data
test <- brownlow_analysis_data |> filter(Season == 2023)
training <- brownlow_analysis_data |> filter(Season < 2023)

##%######################################################%##
#                                                          #
####             Fit model on training data             ####
#                                                          #
##%######################################################%##

# Fit Model
pred_model <-
  polr(
    brownlow_votes ~
      coaches_votes +
      disposal_efficiency +
      contested_possessions*uncontested_possessions +
      kicks +
      handballs +
      marks +
      tackles +
      rating_points +
      metres_gained +
      goals +
      goal_assists +
      score_involvements +
      score_launches + 
      centre_clearances +
      stoppage_clearances +
      intercept_marks +
      intercepts +
      one_percenters +
      pressure_acts +
      inside_fifties +
      rebounds +
      rcs(margin, 4),
    data = training,
    Hess = TRUE
  )

# Generate predicted probabilities
predicted_probabilities <- predict(pred_model, newdata = test, type = "probs")

# Join with test data labels
test_predictions <-
  test |>
  group_by(match, Round) |>
  select(player_name, start_time, match, Round, contains("votes")) |>
  bind_cols(predicted_probabilities) |>
  filter(!is.na(`0`) & !is.na(`1`) & !is.na(`2`) & !is.na(`3`)) |> 
  mutate(`0` = `0` / sum(`0`, na.rm = TRUE),
         `1` = `1` / sum(`1`, na.rm = TRUE),
         `2` = `2` / sum(`2`, na.rm = TRUE),
         `3` = `3` / sum(`3`, na.rm = TRUE)) |>
  mutate(expected_votes = 0*`0` + 1*`1` + 2*`2` + 3*`3`) |>
  arrange(start_time, match, desc(expected_votes)) |>
  slice_head(n = 10) |> 
  mutate(expected_votes = expected_votes * (6/sum(expected_votes))) |> 
  ungroup()

# Get expected counts
test_predictions |>
  group_by(player_name) |>
  summarise(total_expected_votes = sum(expected_votes),
            total_observed_votes = sum(as.numeric(as.character(brownlow_votes)))) |>
  arrange(desc(total_expected_votes))

#===============================================================================
# Simulate medal count
#===============================================================================

# Convert test_predictions to data.table
setDT(test_predictions)

# All match and round combos
unique_matches <- unique(test_predictions[, .(match_name = match, round_name = Round)])

# Function to get votes for a given match
sim_match_votes <- function(match_name, round_name) {
  round_data <- test_predictions[match == match_name & Round == round_name]
  
  # Calculate probabilities in one go
  round_data[, `:=`(three_vote_prob = `3` / sum(`3`),
                    two_vote_prob = (`2` + `3`) / sum(`2` + `3`),
                    one_vote_prob = (`1` + `2` + `3`) / sum(`1` + `2` + `3`))]
  
  # 3 votes
  three_votes <- round_data[sample(.N, 1, prob = three_vote_prob), .(player_name, votes = 3)]
  
  # 2 votes
  two_votes <- round_data[player_name != three_votes$player_name][sample(.N, 1, prob = two_vote_prob), .(player_name, votes = 2)]
  
  # 1 vote
  one_votes <- round_data[!(player_name %in% c(three_votes$player_name, two_votes$player_name))][sample(.N, 1, prob = one_vote_prob), .(player_name, votes = 1)]
  
  # Combine
  rbindlist(list(one_votes, two_votes, three_votes))[, c("match", "round") := .(match_name, round_name)]
}

# Function to map over all matches
run_brownlow_sim <- function(i) {
  full_simulated_count <- unique_matches[, sim_match_votes(match_name, round_name), by = 1:nrow(unique_matches)]
  full_simulated_count[, sim_id := i]
  full_simulated_count
}

sims <-
  map(1:100, run_brownlow_sim) |> 
  bind_rows() |> 
  group_by(player_name, sim_id) |>
  summarise(total = sum(votes)) |> 
  arrange(desc(total))

sims |> 
  group_by(player_name) |> 
  summarise(total_votes = median(total)) |> 
  arrange(desc(total_votes))

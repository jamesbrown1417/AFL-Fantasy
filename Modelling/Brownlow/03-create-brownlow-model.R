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
library(furrr)

# Read in data
brownlow_analysis_data <-
  read_rds("Modelling/Brownlow/brownlow_analysis_data.rds")

# Create votes per game in previous season variable
votes_per_game <-
  brownlow_analysis_data |>
  group_by(player_name, season) |>
  summarise(avg_votes_per_game_prev_season = mean(brownlow_votes, na.rm = TRUE)) |>
  mutate(season = season + 1)

# Career votes
career_votes <-
  brownlow_analysis_data |>
  group_by(player_name) |>
  summarise(career_avg_brownlow_votes = mean(brownlow_votes, na.rm = TRUE))

brownlow_analysis_data <-
  brownlow_analysis_data |>
  mutate(
    brownlow_votes = ifelse(is.na(brownlow_votes), 0, brownlow_votes),
    coaches_votes = ifelse(is.na(coaches_votes), 0, coaches_votes)
  ) |>
  mutate(brownlow_votes = factor(
    brownlow_votes,
    levels = c(0, 1, 2, 3),
    ordered = TRUE
  ))

# Add voting history variable
brownlow_analysis_data <-
  brownlow_analysis_data |>
  left_join(votes_per_game) |>
  left_join(career_votes)

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
      rcs(coaches_votes, 5) +
      disposal_efficiency +
      contested_possessions +
      uncontested_possessions +
      kicks +
      handballs +
      marks +
      tackles +
      rcs(rating_points, 5) +
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
      rcs(career_avg_brownlow_votes, 5) +
      rcs(margin, 5),
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
expected_counts <-
test_predictions |>
  group_by(player_name) |>
  summarise(
    games = n(),
    total_expected_votes = sum(expected_votes),
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

# Set up parallel processing
plan(multisession, workers = 12)

sims <-
  future_map(1:10000, run_brownlow_sim, .progress = TRUE) |> 
  bind_rows() |> 
  group_by(player_name, sim_id) |>
  summarise(total = sum(votes)) |> 
  arrange(desc(total))

# Get player team and name
player_teams <- 
brownlow_analysis_data |> 
  filter(season == 2023) |>
  distinct(player_name, player_team)

# Expected votes
sims |> 
  group_by(player_name) |> 
  summarise(avg_votes = mean(total)) |> 
  arrange(desc(avg_votes))

# Win probability
sims |> 
  arrange(sim_id, desc(total)) |>
  group_by(sim_id) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  group_by(player_name) |> 
  tally() |> 
  arrange(desc(n)) |>
  mutate(win_prob = n / 10000) |>
  mutate(implied_odds = 1/win_prob)

# Team votes winner
sims |> 
  arrange(sim_id, desc(total)) |>
  group_by(sim_id) |>
  left_join(player_teams) |>
  filter(player_team == "Gold Coast Suns") |>
  slice_head(n = 1) |> 
  ungroup() |> 
  group_by(player_name) |> 
  tally() |> 
  arrange(desc(n)) |>
  mutate(win_prob = n / 10000) |>
  mutate(implied_odds = 1/win_prob)



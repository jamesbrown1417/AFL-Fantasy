#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in required data
#===============================================================================

# Get all data
all_data <- read_rds("Modelling/afl_fantasy_data_all.rds")

# Get positions data
positions <-
  read_rds("Data/afl_clustering_positions.rds") |>
  mutate(round = paste("Round", round)) |>
  select(player_name, round, position, position_name)

# Get just 2023 data
fantasy_2023 <-
all_data |>
  filter(season_name == "2023")

# Create training and test splits
training <-
  fantasy_2023 |> 
  filter(round != max(round)) |> 
  filter(round > "Round 10")

test <-
  fantasy_2023 |> 
  filter(round == max(round))

##%######################################################%##
#                                                          #
####                    Add DVP Data                    ####
#                                                          #
##%######################################################%##

# Get DVP for round
get_dvp_round <- function(input_round) {
# Join with position data
current_season_data <-
  fantasy_2023 |> 
  left_join(positions, by = c("player_full_name" = "player_name", "round" = "round"))

# Filter current season data to one before input round
current_season_data <-
  current_season_data |> 
  filter(round < input_round)

# Get list of opposition teams
team_list <-
  current_season_data |>
  distinct(opposition_team) |> 
  pull(opposition_team)

# Get list of positions
position_list <-
  current_season_data |>
  distinct(position_name) |>
  filter(!is.na(position_name)) |> 
  pull(position_name)

# Function to get difference between average vs all other teams vs score vs team
get_dvp <- function(opp_team, pos, n_rounds) {
  
  # Get last n rounds data
  rounds_list <-
    current_season_data |>
    distinct(round) |>
    arrange(desc(round)) |>
    slice_head(n = n_rounds)
  
  # Get positional dataset
  data <-
    current_season_data |>
    filter(round %in% rounds_list$round) |>
    filter(position_name == pos)
  
  # Avg vs all other sides
  vs_others <-
    data |>
    filter(opposition_team != opp_team) |>
    summarise(avg_vs_others = mean(fantasy_points, na.rm = TRUE), .by = player_full_name)
  
  # Avg vs side
  vs_team <-
    data |>
    filter(opposition_team == opp_team) |>
    summarise(avg_vs_team = mean(fantasy_points, na.rm = TRUE), .by = player_full_name)
  
  # Join together
  all_data <-
    inner_join(vs_others, vs_team)
  
  # Get dvp score
  score = mean(all_data$avg_vs_team) - mean(all_data$avg_vs_others)
  score = round(score, 2)
  
  # Get percentage scoring over their avg vs team_list
  percentage = mean(all_data$avg_vs_team > all_data$avg_vs_others)
  percentage = round(percentage * 100, 2)
  
  tibble(opposition_team = opp_team, dvp = score, percentage_over_avg = percentage)
}

# Create df of all combinations
dvp_data <-
  expand_grid(opp_team = team_list,
              pos = position_list,
              n_rounds = c(10))

# Apply function to each combination of the dataframe columns
dvp_output <-
  pmap_df(.l = dvp_data, .f = get_dvp) |>
  cbind(dvp_data[,-1])

# Add round number to dvp output
dvp_output$round <- input_round

return(dvp_output)
  }

# Get list of rounds to consider
rounds_to_consider <- unique(training$round)

# Apply function to each round in the training data
all_dvp <-
  map(rounds_to_consider, get_dvp_round) |> 
  bind_rows()

##%######################################################%##
#                                                          #
####                 Get past n average                 ####
#                                                          #
##%######################################################%##

get_last_n_avgs <- function(input_round) {
  player_data <-
  fantasy_2023 |> 
    filter(round < input_round) |> 
    arrange(player_full_name, desc(round))
  
  last_3_avg <- player_data |> group_by(player_full_name) |> filter(n() >= 3) |>  slice_head(n = 3) |>  summarise(last_3_avg = mean(fantasy_points))
  last_5_avg <- player_data |> group_by(player_full_name) |> filter(n() >= 5) |>slice_head(n = 5) |>  summarise(last_5_avg = mean(fantasy_points))
  last_7_avg <- player_data |> group_by(player_full_name) |> filter(n() >= 7) |>slice_head(n = 7) |>  summarise(last_7_avg = mean(fantasy_points))
  season_avg <- player_data |> group_by(player_full_name) |> filter(n() > 7) |> summarise(season_avg = mean(fantasy_points))
  
  output <- (last_3_avg |> full_join(last_5_avg) |> full_join(last_7_avg) |> full_join(season_avg))
  output$round <- input_round
  
  return(output)
}

get_last_n_avgs("Round 17")

# Apply function to each round
averages_output <-
  map(rounds_to_consider, get_last_n_avgs) |> 
  bind_rows()

# Get only complete cases
averages_output <- averages_output[complete.cases(averages_output), ]

##%######################################################%##
#                                                          #
####      Get most common position in last 5 games      ####
#                                                          #
##%######################################################%##

# Add most common position in last 5 games

# Make round number an ordered factor
positions$round <-
  factor(
    positions$round,
    levels = c(
      'Round 1',
      'Round 2',
      'Round 3',
      'Round 4',
      'Round 5',
      'Round 6',
      'Round 7',
      'Round 8',
      'Round 9',
      'Round 10',
      'Round 11',
      'Round 12',
      'Round 13',
      'Round 14',
      'Round 15',
      'Round 16',
      'Round 17',
      'Round 18',
      'Round 19',
      'Round 20',
      'Round 21',
      'Round 22',
      'Round 23',
      'Round 24',
      'Finals Week 1',
      'Semi Finals',
      'Preliminary Finals',
      'Grand Final'
    ),
    ordered = TRUE
  )

# Function to get position
get_position <- function(input_round) {
  output <-
  positions |> 
    filter(round < input_round) |> 
    arrange(desc(round)) |> 
    group_by(player_name) |> 
    filter(n() >= 5) |> 
    slice_head(n = 5) |> 
    group_by(player_name, position_name) |> 
    tally() |>
    arrange(player_name, desc(n)) |> 
    group_by(player_name) |> 
    slice_head(n = 1) |> 
    ungroup() |> 
    rename(player_full_name = player_name) |> 
    select(-n)
  
  output$round <- input_round
  
  return(output)
}

# Apply function to each round
positions_output <-
  map(rounds_to_consider, get_position) |> 
  bind_rows() |> 
  rename(pos = position_name)

##%######################################################%##
#                                                          #
####             Combine together to create             ####
####             training and test datasets             ####
#                                                          #
##%######################################################%##

#===============================================================================
# Training
#===============================================================================

training <-
training |> 
  mutate(home_away = player_team == home_team) |> 
  select(player_full_name, player_team, opposition_team, round, fantasy_points, home_away) |> 
  left_join(averages_output) |> 
  left_join(positions_output) |> 
  left_join(all_dvp) |> 
  select(-n_rounds, -pos)

training <- training[complete.cases(training), ]

# Make home_away binary
training <-
  training |> 
  rename(home_game = home_away) |> 
  mutate(home_game = as.numeric(home_game))


#===============================================================================
# Test
#===============================================================================

# round number
test_round <- test$round[[1]]

test <-
  test |>
  mutate(home_away = player_team == home_team) |>
  select(player_full_name,
         player_team,
         opposition_team,
         round,
         fantasy_points,
         home_away) |> 
  left_join(get_last_n_avgs(test_round)) |> 
  left_join(get_position(test_round) |> rename(pos = position_name)) |> 
  left_join(get_dvp_round(test_round)) |> 
  select(-n_rounds, -pos)

test <- test[complete.cases(test), ]

# Make home_away binary
test <-
  test |> 
  rename(home_game = home_away) |> 
  mutate(home_game = as.numeric(home_game))

#===============================================================================
# Target Data
#===============================================================================

fixture <- fitzRoy::fetch_fixture_afl()

latest_round_1 <- 
  fixture |>
  filter(compSeason.currentRoundNumber == round.roundNumber) |> 
  select(home_team = home.team.name, away_team = away.team.name, round = round.name) |> 
  mutate(player_team = home_team)

latest_round_2 <- 
  fixture |>
  filter(compSeason.currentRoundNumber == round.roundNumber) |> 
  select(home_team = home.team.name, away_team = away.team.name, round = round.name) |> 
  mutate(player_team = away_team)

latest_round <-
  bind_rows(latest_round_1, latest_round_2) |> 
  mutate(home_away = player_team == home_team) |> 
  rename(home_game = home_away) |> 
  mutate(home_game = as.numeric(home_game)) |> 
  mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
  select(player_team, opposition_team, round, home_game)

# Get players and player team var
target <-
  training |>
  distinct(player_full_name, player_team) |> 
  left_join(latest_round)

# round number
target_round <- target$round[[1]]

target <-
  target |>
  select(player_full_name,
         player_team,
         opposition_team,
         round,
         home_game) |> 
  left_join(get_last_n_avgs(target_round)) |> 
  left_join(get_position(target_round) |> rename(pos = position_name)) |> 
  left_join(get_dvp_round(target_round)) |> 
  select(-n_rounds, -pos)

target <- target[complete.cases(target), ]

#===============================================================================
# Write out as CSVs
#===============================================================================

training |> write_csv("Modelling/Predictive-Models/training_data.csv")
test |> write_csv("Modelling/Predictive-Models/test_data.csv")
target |> write_csv("Modelling/Predictive-Models/target_data.csv")

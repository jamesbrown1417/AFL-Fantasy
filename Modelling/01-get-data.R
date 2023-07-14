##%######################################################%##
#                                                          #
####                 Get the data used                  ####
####         for modelling of player disposals          ####
#                                                          #
##%######################################################%##

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in data
#===============================================================================

# Get helper functions
source("Functions/data_processing_functions.r")

# Get data
data_2023 <- get_fantasy_data(season = 2023)
afl_fantasy_2014_2022_data <- readRDS("Data/afl_fantasy_2014_2022_data.rds")

# Combine
afl_fantasy_data <-
  bind_rows(data_2023, afl_fantasy_2014_2022_data) |> 
  arrange(start_time_utc)

# Make round number an ordered factor
afl_fantasy_data$round <-
  factor(
    afl_fantasy_data$round,
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

#===============================================================================
# Write out
#===============================================================================

afl_fantasy_data |> 
  write_rds("Modelling/afl_fantasy_data_all.rds")

bailey_dale <-
afl_fantasy_data |>
  filter(player_full_name == "Caleb Daniel") |>
  mutate(home = player_team == home_team) |>
  select(match_name,
  start_time = start_time_utc,
  player_team,
  opposition_team,
  home,
   venue,
    start_time_utc,
     season_name,
      round,
       temperature,
        weather_category,
         player_full_name,
          fantasy_points,
           disposals) |>
           mutate(start_time = start_time + hours(10)) |>
           mutate(start_time = format(start_time, format = "%H:%M:%S")) |>
           mutate(day_or_night = ifelse(start_time < "18:00:00", "Day", "Night")) |>
           mutate(day_or_night = ifelse(venue == "Marvel Stadium", "Under the Roof", day_or_night)) |>
           mutate(weather_category = ifelse(venue == "Marvel Stadium", "Under the Roof", weather_category)) |>
           filter(season_name %in% c("2021", "2022", "2023")) |>
           group_by(day_or_night) |>
           summarise(games_played = n(), avg_disposals = mean(disposals), avg_fantasy_points = mean(fantasy_points)) |>
           arrange(desc(avg_disposals))
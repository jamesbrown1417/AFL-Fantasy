# Get functions to determine player correlations based on historical data

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(tidytable)
library(stringr)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

source("Functions/data_processing_functions.R")

str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

#===============================================================================
# Get historical data
#===============================================================================

# 2014 - 2022
afl_fantasy_2014_2022_data <- readRDS("C:/Users/a1645197/AFL-Fantasy/Data/afl_fantasy_2014_2022_data.rds")

# 2023
afl_fantasy_2023_data <- get_fantasy_data(season = 2023)

# Combine
player_stats <- bind_rows(afl_fantasy_2014_2022_data, afl_fantasy_2023_data)

# Get margin to be negative if player team lost
player_stats <-
  player_stats |> 
  select(player_full_name,
         tog_percentage,
         season_name,
         round,
         player_team,
         home_team,
         away_team,
         match_result,
         margin,
         disposals,
         tackles,
         fantasy_points) |> 
  mutate(margin = if_else(player_team == away_team & match_result == "Home Win", -margin, margin)) |> 
  mutate(margin = if_else(player_team == home_team & match_result == "Away Win", -margin, margin)) 

#=================================================================================
# Get unique players and map function to find correlation between margin and stats
#=================================================================================

# Unique players
unique_players <-
  player_stats |> 
  filter(season_name == 2023) |> 
  group_by(player_full_name) |> 
  filter(n() > 5) |> 
  ungroup() |> 
  distinct(player_full_name)

# Function to get correlation
get_margin_corr <- function(player_name) {
  player_stats |>
    filter(tog_percentage >= 60) |> 
    filter(player_full_name == player_name) |> 
    summarise(
      player = max(player_full_name),
      games = n(),
      correlation = cor.test(margin, disposals)$estimate,
      pvalue = cor.test(margin, disposals)$p.value)
}

# Map function to unique_players
margin_correlations <-
  map_dfr(unique_players$player_full_name, get_margin_corr) |>
  filter(games >= 20) |> 
  mutate(abs_correlation = abs(correlation)) |> 
  arrange(desc(abs_correlation)) |> 
  select(-abs_correlation)

# Function to plot correlation
plot_margin_corr <- function(player_name) {
  player_stats |>
    filter(tog_percentage >= 60) |> 
    filter(player_full_name == player_name) |> 
    ggplot(aes(x = margin, y = disposals)) +
    geom_point() +
    geom_smooth()
}

plot_margin_corr("Jeremy Cameron")

#===============================================================================
# Functions for fantasy points
#===============================================================================

# Function to get correlation
get_margin_corr_fantasy <- function(player_name) {
  player_stats |>
    filter(tog_percentage >= 60) |> 
    filter(player_full_name == player_name) |> 
    summarise(
      player = max(player_full_name),
      games = n(),
      correlation = cor.test(margin, fantasy_points)$estimate,
      pvalue = cor.test(margin, fantasy_points)$p.value)
}

# Map function to unique_players
margin_correlations_fantasy <-
  map_dfr(unique_players$player_full_name, get_margin_corr_fantasy) |>
  filter(games >= 20) |> 
  mutate(abs_correlation = abs(correlation)) |> 
  arrange(desc(abs_correlation)) |> 
  select(-abs_correlation)

# Function to plot correlation
plot_margin_corr_fantasy <- function(player_name) {
  player_stats |>
    filter(tog_percentage >= 60) |> 
    filter(player_full_name == player_name) |> 
    ggplot(aes(x = margin, y = fantasy_points)) +
    geom_point() +
    geom_smooth(method = "lm")
}



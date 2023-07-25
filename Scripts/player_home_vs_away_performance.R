# Libraries and functions
library(tidyverse)
library(qreport)
library(kableExtra)
library(fitzRoy)

`%notin%` <- Negate(`%in%`)

# Load data processing functions
source("Functions/data_processing_functions.R")

#===============================================================================
# Get historical data
#===============================================================================

# 2014 - 2022
afl_fantasy_2014_2022_data <- readRDS("C:/Users/a1645197/AFL-Fantasy/Data/afl_fantasy_2014_2022_data.rds")

# 2023
afl_fantasy_2023_data <- get_fantasy_data(season = 2023)

# Combine
player_stats <-
 bind_rows(afl_fantasy_2014_2022_data, afl_fantasy_2023_data) |>
 mutate(home_away = ifelse(player_team == home_team, "Home", "Away")) |>
 filter(season_name %in% c("2023", "2022", "2021"))

#===============================================================================
# Get biggest home/away differences
#===============================================================================

# Get home/away differences
home_avgs <-
    player_stats |>
    filter(home_away == "Home") |>
    group_by(player_full_name) |>
    summarise(
        home_games_played = n(),
        home_avg_disposals = mean(disposals, na.rm = TRUE),
        home_avg_fantasy_points = mean(fantasy_points, na.rm = TRUE),
        `15+ Disposals Home` = mean(disposals >= 15),
        `20+ Disposals Home` = mean(disposals >= 20),
        `25+ Disposals Home` = mean(disposals >= 25),
        `30+ Disposals Home` = mean(disposals >= 30)
    )

away_avgs <-
    player_stats |>
    filter(home_away == "Away") |>
    group_by(player_full_name) |>
    summarise(
        away_games_played = n(),
        away_avg_disposals = mean(disposals, na.rm = TRUE),
        away_avg_fantasy_points = mean(fantasy_points, na.rm = TRUE),
        `15+ Disposals Away` = mean(disposals >= 15),
        `20+ Disposals Away` = mean(disposals >= 20),
        `25+ Disposals Away` = mean(disposals >= 25),
        `30+ Disposals Away` = mean(disposals >= 30)
    )

# Join
home_away_avgs <-
    inner_join(home_avgs, away_avgs, by = "player_full_name") |>
    mutate(disposals_diff = home_avg_disposals - away_avg_disposals,
    fantasy_points_diff = home_avg_fantasy_points - away_avg_fantasy_points,
    `15+ Disposals Diff` = `15+ Disposals Home` - `15+ Disposals Away`,
    `20+ Disposals Diff` = `20+ Disposals Home` - `20+ Disposals Away`,
    `25+ Disposals Diff` = `25+ Disposals Home` - `25+ Disposals Away`,
    `30+ Disposals Diff` = `30+ Disposals Home` - `30+ Disposals Away`,
    ) |>
    arrange(desc(disposals_diff)) |>
    filter(home_games_played >= 10  & away_games_played >= 10) |>
    filter(player_full_name %in% afl_fantasy_2023_data$player_full_name)


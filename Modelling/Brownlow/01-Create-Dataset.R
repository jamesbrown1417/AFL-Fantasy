# Load libraries
library(tidyverse)
library(fitzRoy)

##%######################################################%##
#                                                          #
####                      Get data                      ####
#                                                          #
##%######################################################%##

# Get player stats--------------------------------------------------------------
afl_stats_2014 <- fetch_player_stats_afl(season = 2014)
afl_stats_2015 <- fetch_player_stats_afl(season = 2015)
afl_stats_2016 <- fetch_player_stats_afl(season = 2016)
afl_stats_2017 <- fetch_player_stats_afl(season = 2017)
afl_stats_2018 <- fetch_player_stats_afl(season = 2018)
afl_stats_2019 <- fetch_player_stats_afl(season = 2019)
afl_stats_2020 <- fetch_player_stats_afl(season = 2020)
afl_stats_2021 <- fetch_player_stats_afl(season = 2021)
afl_stats_2022 <- fetch_player_stats_afl(season = 2022)
afl_stats_2023 <- fetch_player_stats_afl(season = 2023)

afl_stats <-
  bind_rows(
    afl_stats_2014,
    afl_stats_2015,
    afl_stats_2016,
    afl_stats_2017,
    afl_stats_2018,
    afl_stats_2019,
    afl_stats_2020,
    afl_stats_2021,
    afl_stats_2022,
    afl_stats_2023
  )

# Get Brownlow votes------------------------------------------------------------
brownlow_votes_2014 <- fetch_player_stats_afltables(season = 2014)
brownlow_votes_2015 <- fetch_player_stats_afltables(season = 2015)
brownlow_votes_2016 <- fetch_player_stats_afltables(season = 2016)
brownlow_votes_2017 <- fetch_player_stats_afltables(season = 2017)
brownlow_votes_2018 <- fetch_player_stats_afltables(season = 2018)
brownlow_votes_2019 <- fetch_player_stats_afltables(season = 2019)
brownlow_votes_2020 <- fetch_player_stats_afltables(season = 2020)
brownlow_votes_2021 <- fetch_player_stats_afltables(season = 2021)
brownlow_votes_2022 <- fetch_player_stats_afltables(season = 2022)

# Combine
brownlow_votes <-
  bind_rows(
    brownlow_votes_2014,
    brownlow_votes_2015,
    brownlow_votes_2016,
    brownlow_votes_2017,
    brownlow_votes_2018,
    brownlow_votes_2019,
    brownlow_votes_2020,
    brownlow_votes_2021,
    brownlow_votes_2022
  )

# Coaches Votes-----------------------------------------------------------------
coaches_votes_2014 <- fetch_coaches_votes(season = 2014)
coaches_votes_2015 <- fetch_coaches_votes(season = 2015)
coaches_votes_2016 <- fetch_coaches_votes(season = 2016)
coaches_votes_2017 <- fetch_coaches_votes(season = 2017)
coaches_votes_2018 <- fetch_coaches_votes(season = 2018)
coaches_votes_2019 <- fetch_coaches_votes(season = 2019)
coaches_votes_2020 <- fetch_coaches_votes(season = 2020)
coaches_votes_2021 <- fetch_coaches_votes(season = 2021)
coaches_votes_2022 <- fetch_coaches_votes(season = 2022)
coaches_votes_2023 <- fetch_coaches_votes(season = 2023)

# Combine
coaches_votes <- 
  bind_rows(
    coaches_votes_2014,
    coaches_votes_2015,
    coaches_votes_2016,
    coaches_votes_2017,
    coaches_votes_2018,
    coaches_votes_2019,
    coaches_votes_2020,
    coaches_votes_2021,
    coaches_votes_2022,
    coaches_votes_2023
  )

# Tidy

##%######################################################%##
#                                                          #
#### select the variables deemed relevant for modelling ####
#                                                          #
##%######################################################%##

afl_stats_2023_clustering <-
  afl_stats_2023 |>
  select(
    player_first_name = player.player.player.givenName,
    player_last_name = player.player.player.surname,
    kicks,
    marks,
    handballs,
    goals,
    behinds,
    hitouts,
    tackles,
    rebounds = rebound50s,
    inside_fifties = inside50s,
    contested_possessions = contestedPossessions,
    uncontested_possessions = uncontestedPossessions,
    marks_inside_fifty = marksInside50,
    one_percenters = onePercenters,
    bounces,
    goal_assists = goalAssists,
    # time_on_ground_percentage,
    centre_clearances = clearances.centreClearances,
    stoppage_clearances = clearances.stoppageClearances,
    score_involvements = scoreInvolvements,
    metres_gained = metresGained,
    intercepts,
    tackles_inside_fifty = tacklesInside50,
    contest_def_losses = extendedStats.contestDefLosses,
    contest_def_one_on_ones = extendedStats.contestDefOneOnOnes,
    contest_off_one_on_ones = extendedStats.contestOffOneOnOnes,
    contest_off_wins = extendedStats.contestOffWins,
    def_half_pressure_acts = extendedStats.defHalfPressureActs,
    f50_ground_ball_gets = extendedStats.f50GroundBallGets,
    ground_ball_gets = extendedStats.groundBallGets,
    intercept_marks = extendedStats.interceptMarks,
    marks_on_lead = extendedStats.marksOnLead,
    pressure_acts = extendedStats.pressureActs,
    ruck_contests = extendedStats.ruckContests,
    score_launches= extendedStats.scoreLaunches,
    shots_at_goal = shotsAtGoal,
    spoils = extendedStats.spoils,
    kickins = extendedStats.kickins,
    centre_bounces = extendedStats.centreBounceAttendances
  ) |>
  mutate(player_name = paste(player_first_name, player_last_name, sep = " ")) |>
  select(-player_first_name, -player_last_name)

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

# Fix round
brownlow_votes$Round <- paste("Round", brownlow_votes$Round)

# Create player_name variable
brownlow_votes <-
  brownlow_votes |>
  transmute(
    match = paste(Home.team, "v", Away.team),
    Season = as.integer(Season),
    Round,
    player_name = paste(First.name, Surname),
    player_team = Playing.for,
    brownlow_votes = Brownlow.Votes
  )

# Fix team names
brownlow_votes <-
  brownlow_votes |> 
  mutate(match = str_replace_all(match, "Adelaide", "Adelaide Crows")) |>
  mutate(match = str_replace_all(match, "Geelong", "Geelong Cats")) |>
  mutate(match = str_replace_all(match, "Gold Coast", "Gold Coast Suns")) |>
  mutate(match = str_replace_all(match, "Greater Western Sydney", "GWS Giants")) |>
  mutate(match = str_replace_all(match, "^Sydney", "Sydney Swans")) |>
  mutate(match = str_replace_all(match, "West Coast", "West Coast Eagles")) |>
  mutate(player_name = str_replace_all(player_name, "Jordan de Goey", "Jordan De Goey")) |>
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "Sydney Swans", "Josh P. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "West Coast Eagles", "Josh J. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Tom Lynch" & player_team == "Richmond", "Tom J. Lynch", player_name)) |>
  mutate(player_name = ifelse(player_name == "Tom Lynch" & player_team == "Gold Coast Suns", "Tom J. Lynch", player_name)) |>
  mutate(player_name = str_to_title(player_name))
  
##%######################################################%##
#                                                          #
####             Read in coaches votes data             ####
#                                                          #
##%######################################################%##

coaches_votes <-
  read_rds("Modelling/Brownlow/coaches_votes_2014_to_2023.rds") |>
  mutate(season = as.integer(season)) |>
  rename(player_name = player_full_name) |>
  rename(Season = season, Round = round)

##%######################################################%##
#                                                          #
#### select the variables deemed relevant for modelling ####
#                                                          #
##%######################################################%##

afl_stats_brownlow <-
  afl_stats |>
  mutate(match = paste(home.team.name, "v", away.team.name)) |>
  select(
    match,
    player_first_name = player.player.player.givenName,
    player_last_name = player.player.player.surname,
    start_time = utcStartTime,
    Round = round.name,
    kicks,
    marks,
    handballs,
    goals,
    behinds,
    hitouts,
    tackles,
    disposal_efficiency = disposalEfficiency,
    rebounds = rebound50s,
    inside_fifties = inside50s,
    contested_possessions = contestedPossessions,
    uncontested_possessions = uncontestedPossessions,
    marks_inside_fifty = marksInside50,
    one_percenters = onePercenters,
    bounces,
    goal_assists = goalAssists,
    time_on_ground_percentage = timeOnGroundPercentage,
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
    turnovers,
    rating_points = ratingPoints,
    centre_bounces = extendedStats.centreBounceAttendances
  ) |>
  mutate(player_name = paste(player_first_name, player_last_name, sep = " ")) |>
  select(-player_first_name, -player_last_name) |>
  relocate(player_name, .after = Round) |>
  filter(time_on_ground_percentage >= 40)

##%######################################################%##
#                                                          #
####         Normalise variables by match total         ####
#                                                          #
##%######################################################%##

afl_stats_brownlow_analysis <-
  afl_stats_brownlow |>
  mutate(Season = year(start_time)) |>
  relocate(player_name, match, Season, Round, .before = start_time) |>
  left_join(brownlow_votes) |>
  left_join(coaches_votes) |>
  relocate(brownlow_votes, coaches_votes, .after = Round) |>
  group_by(Season, Round, match) |>
  mutate(kicks = kicks / sum(kicks, na.rm = TRUE),
         marks = marks / sum(marks, na.rm = TRUE),
         handballs = handballs / sum(handballs, na.rm = TRUE),
         goals = goals / sum(goals, na.rm = TRUE),
         hitouts = hitouts / sum(hitouts, na.rm = TRUE),
         tackles = tackles / sum(tackles, na.rm = TRUE),
         disposal_efficiency,
         rebounds = rebounds / sum(rebounds, na.rm = TRUE),
         inside_fifties = inside_fifties / sum(inside_fifties, na.rm = TRUE),
         contested_possessions = contested_possessions / sum(contested_possessions, na.rm = TRUE),
         uncontested_possessions = uncontested_possessions / sum(uncontested_possessions, na.rm = TRUE),
         marks_inside_fifty = marks_inside_fifty / sum(marks_inside_fifty, na.rm = TRUE),
         one_percenters = one_percenters / sum(one_percenters, na.rm = TRUE),
         centre_clearances = centre_clearances / sum(centre_clearances, na.rm = TRUE),
         stoppage_clearances = stoppage_clearances / sum(stoppage_clearances, na.rm = TRUE),
         score_involvements = score_involvements / sum(score_involvements, na.rm = TRUE),
         metres_gained,
         intercepts = intercepts / sum(intercepts, na.rm = TRUE),
         ground_ball_gets = ground_ball_gets / sum(ground_ball_gets, na.rm = TRUE),
         intercept_marks = intercept_marks / sum(intercept_marks, na.rm = TRUE),
         pressure_acts = pressure_acts / sum(pressure_acts, na.rm = TRUE),
         score_launches = score_launches / sum(score_launches, na.rm = TRUE),
         turnovers = turnovers / sum(turnovers, na.rm = TRUE),
         rating_points) |>
  ungroup()

##%######################################################%##
#                                                          #
####                  Write out as RDS                  ####
#                                                          #
##%######################################################%##

afl_stats_brownlow_analysis |>
  write_rds("Modelling/Brownlow/brownlow_analysis_data.rds")


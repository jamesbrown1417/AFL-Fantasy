##%######################################################%##
#                                                          #
####             Data set up and functions              ####
#                                                          #
##%######################################################%##

#===============================================================================
# Libraries
#===============================================================================

library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Functions to get data
#===============================================================================

# Data
afl_fantasy_data_all <- read_rds("../afl_fantasy_data_all.rds")

# Make margin variable negative if loss
afl_fantasy_data_all <-
  afl_fantasy_data_all |> 
  mutate(margin = ifelse(player_team == home_team & match_result == "Away Win", -margin, margin)) |> 
  mutate(margin = ifelse(player_team == away_team & match_result == "Home Win", -margin, margin))

# Create result category variable
afl_fantasy_data_all$result_category <- cut(
  afl_fantasy_data_all$margin,
  breaks = c(-Inf,-40,-1, 0, 39, Inf),
  labels = c("40+ Loss", "1-39 Loss", "Draw", "1-39 Win", "40+ Win"),
  right = TRUE,
  include.lowest = TRUE
)

afl_fantasy_data_all <-
  afl_fantasy_data_all |> 
  relocate(result_category, .after = margin) |>
  mutate(cba_percentage = round(cba_percentage * 100, 1))

# Player history table
get_player_data <- function(player_name, seasons = NULL) {
  data <-
    afl_fantasy_data_all |>
    select(
      player_team,
      home_team,
      opposition_team,
      venue,
      season = season_name,
      round,
      start_time = start_time_utc,
      temperature,
      weather_category,
      margin,
      result_category,
      player_full_name,
      goals,
      behinds,
      kicks,
      handballs,
      marks,
      tackles,
      disposals,
      fantasy_points,
      hitouts,
      tog_percentage,
      cba_percentage,
    ) |> 
    mutate(start_time = with_tz(start_time, tzone = "Australia/Melbourne")) |> 
    mutate(home_away = ifelse(player_team == home_team, "Home", "Away")) |> 
    mutate(day_or_night = ifelse(hour(start_time) >= 18 | hour(start_time) < 6, "Night", "Day")) |>
    mutate(weather_category = ifelse(venue == "Marvel Stadium", "ROOF_CLOSED", weather_category)) |> 
    mutate(weather_category = fct_collapse(weather_category,
                                           Clear = c("MOSTLY_SUNNY", "MOSTLY_CLEAR", "SUNNY", "CLEAR_NIGHT"),
                                           Overcast = c("OVERCAST"),
                                           Rain = c("RAIN", "THUNDERSTORMS"),
                                           Windy = c("WINDY"),
                                           Indoors = c("ROOF_CLOSED")))
  
  data <- data |> filter(player_full_name == player_name)
  
  if (!is.null(seasons)) {
    data <- data |> filter(season %in% seasons)
  }
  
  return(data)
}

# Player Stat Summary
player_stat_summary <- function(data, grouping_vars) {
  data |>
    group_by(across(all_of(grouping_vars))) |> 
    summarise(
      games = n(),
      avg_disposals = mean(disposals) |> round(2),
      med_disposals = median(disposals) |> round(2),
      `15+ %` = mean(disposals >= 15) |> round(2),
      `20+ %` = mean(disposals >= 20) |> round(2),
      `25+ %` = mean(disposals >= 25) |> round(2),
      `30+ %` = mean(disposals >= 30) |> round(2),
      `35+ %` = mean(disposals >= 35) |> round(2),
      `70+ FP` = mean(fantasy_points >= 70) |> round(2),
      `70+ FP` = mean(fantasy_points >= 70) |> round(2),
      `80+ FP` = mean(fantasy_points >= 80) |> round(2),
      `90+ FP` = mean(fantasy_points >= 90) |> round(2),
      `100+ FP` = mean(fantasy_points >= 100) |> round(2),
      `110+ FP` = mean(fantasy_points >= 110) |> round(2),
      `120+ FP` = mean(fantasy_points >= 120) |> round(2),
      avg_fantasy = mean(fantasy_points) |> round(2),
      med_fantasy = median(fantasy_points) |> round(2),
      avg_kicks = mean(kicks) |> round(2),
      avg_handballs = mean(handballs) |> round(2),
      avg_marks = mean(marks) |> round(2),
      avg_tackles = mean(tackles) |> round(2),
      avg_goals = mean(goals) |> round(2),
      `TOG %` = mean(tog_percentage) |> round(2),
      `CBA %` = mean(cba_percentage) |> round(2)
    ) |> 
    arrange(desc(avg_fantasy))
}

# With / without teammate
with_without <- function(player, teammate, season) {
  full_data <-
    afl_fantasy_data_all |>
    filter(season_name %in% season) |> 
    filter(tog_percentage >= 50) |> 
    filter(player_full_name == player | player_full_name == teammate)
  
  # Games with both
  both <-
    full_data |>
    group_by(match_name, round, season_name, player_team) |> 
    filter(n() == 2) |> 
    ungroup() |> 
    filter(player_full_name == player) |> 
    group_by(player_full_name) |> 
    summarise(
      games = n(),
      avg_disposals = mean(disposals) |> round(2),
      med_disposals = median(disposals) |> round(2),
      `15+ %` = mean(disposals >= 15) |> round(2),
      `20+ %` = mean(disposals >= 20) |> round(2),
      `25+ %` = mean(disposals >= 25) |> round(2),
      `30+ %` = mean(disposals >= 30) |> round(2),
      `35+ %` = mean(disposals >= 35) |> round(2),
      `70+ FP` = mean(fantasy_points >= 70) |> round(2),
      `70+ FP` = mean(fantasy_points >= 70) |> round(2),
      `80+ FP` = mean(fantasy_points >= 80) |> round(2),
      `90+ FP` = mean(fantasy_points >= 90) |> round(2),
      `100+ FP` = mean(fantasy_points >= 100) |> round(2),
      `110+ FP` = mean(fantasy_points >= 110) |> round(2),
      `120+ FP` = mean(fantasy_points >= 120) |> round(2),
      avg_fantasy = mean(fantasy_points) |> round(2),
      med_fantasy = median(fantasy_points) |> round(2),
      avg_kicks = mean(kicks) |> round(2),
      avg_handballs = mean(handballs) |> round(2),
      avg_marks = mean(marks) |> round(2),
      avg_tackles = mean(tackles) |> round(2),
      avg_goals = mean(goals) |> round(2),
      `TOG %` = mean(tog_percentage) |> round(2),
      `CBA %` = mean(cba_percentage) |> round(2)
    ) |> 
    arrange(desc(avg_fantasy)) |> 
    mutate(with_teammate = TRUE,
           teammate = teammate) |> 
    relocate(teammate, with_teammate, .after = player_full_name)
  
  
  # Games with just selected player
  just_player <-
    full_data |>
    group_by(match_name, round, season_name, player_team) |> 
    filter(n() == 1) |> 
    ungroup() |> 
    filter(player_full_name == player) |> 
    group_by(player_full_name) |> 
    summarise(
      games = n(),
      avg_disposals = mean(disposals) |> round(2),
      med_disposals = median(disposals) |> round(2),
      `15+ %` = mean(disposals >= 15) |> round(2),
      `20+ %` = mean(disposals >= 20) |> round(2),
      `25+ %` = mean(disposals >= 25) |> round(2),
      `30+ %` = mean(disposals >= 30) |> round(2),
      `35+ %` = mean(disposals >= 35) |> round(2),
      `70+ FP` = mean(fantasy_points >= 70) |> round(2),
      `70+ FP` = mean(fantasy_points >= 70) |> round(2),
      `80+ FP` = mean(fantasy_points >= 80) |> round(2),
      `90+ FP` = mean(fantasy_points >= 90) |> round(2),
      `100+ FP` = mean(fantasy_points >= 100) |> round(2),
      `110+ FP` = mean(fantasy_points >= 110) |> round(2),
      `120+ FP` = mean(fantasy_points >= 120) |> round(2),
      avg_fantasy = mean(fantasy_points) |> round(2),
      med_fantasy = median(fantasy_points) |> round(2),
      avg_kicks = mean(kicks) |> round(2),
      avg_handballs = mean(handballs) |> round(2),
      avg_marks = mean(marks) |> round(2),
      avg_tackles = mean(tackles) |> round(2),
      avg_goals = mean(goals) |> round(2),
      `TOG %` = mean(tog_percentage) |> round(2),
      `CBA %` = mean(cba_percentage) |> round(2)
    ) |> 
    arrange(desc(avg_fantasy)) |> 
    mutate(with_teammate = FALSE,
           teammate = teammate) |> 
    relocate(teammate, with_teammate, .after = player_full_name)
  
  # Return summary
  bind_rows(just_player, both)
}

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  titlePanel("Player History"),
  theme = shinytheme("united"),
  sidebarLayout(
    sidebarPanel(
      textInput("player_name", "Enter Player's Name"),
      checkboxInput("remove_injured", label = "Remove Games with <50% TOG?", value = TRUE),
      selectInput(
        "season_name",
        "Season",
        choices = unique(afl_fantasy_data_all$season_name),
        selectize = TRUE,
        multiple = TRUE,
        selected = c("2021", "2022", "2023")
      ),
      checkboxGroupInput(
        "group_vars",
        "Summarise By",
        choiceNames = c("Venue", "Day / Night", "Home / Away", "Opposition Team", "Weather", "Win / Loss"),
        choiceValues = c("venue", "day_or_night", "home_away", "opposition_team", "weather_category", "result_category")
      ),
      textInput("team_mate", "Compare Performance With and Without Teammate"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Player Stats", 
                 h3("Player Stats Table"),
                 dataTableOutput("player_stats_table")
        ),
        tabPanel("Player Stats Summary", 
                 h3("Summary"),
                 dataTableOutput("summary_table")
        ),
        tabPanel("With and Without Teammate", 
                 h3("Summary"),
                 dataTableOutput("with_without_table")
        )
      )
    )
  )
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output) {
  output$player_stats_table <- renderDataTable({
    if (input$remove_injured) {
      get_player_data(input$player_name, input$season_name) |>
        filter(tog_percentage >= 50)
    }
    
    else {
      get_player_data(input$player_name, input$season_name)
    }
  })
  
  output$summary_table <- renderDataTable({
    if (input$remove_injured) {
      summ_dat <-
        get_player_data(input$player_name, input$season_name) |>
        filter(tog_percentage >= 50)
      player_stat_summary(summ_dat, c(input$group_vars))
    }
    
    else{
      summ_dat <- get_player_data(input$player_name, input$season_name)
      player_stat_summary(summ_dat, c(input$group_vars))
    }
    
  })
  
  output$with_without_table <- renderDataTable({
    with_without(input$player_name, input$team_mate, input$season_name)
  })
}

##%######################################################%##
#                                                          #
####                       Run                          ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)

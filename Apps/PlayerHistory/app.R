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
      avg_disposals = mean(disposals) |> round(3),
      `15+ %` = mean(disposals >= 15) |> round(3),
      `20+ %` = mean(disposals >= 20) |> round(3),
      `25+ %` = mean(disposals >= 25) |> round(3),
      `30+ %` = mean(disposals >= 30) |> round(3),
      `35+ %` = mean(disposals >= 35) |> round(3),
      avg_fantasy = mean(fantasy_points) |> round(3),
      avg_kicks = mean(kicks) |> round(3),
      avg_handballs = mean(handballs) |> round(3),
      avg_marks = mean(marks) |> round(3),
      avg_tackles = mean(tackles) |> round(3)
    ) |> 
    arrange(desc(avg_fantasy))
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
        choiceNames = c("Venue", "Day / Night", "Home / Away", "Opposition Team", "Weather"),
        choiceValues = c("venue", "day_or_night", "home_away", "opposition_team", "weather_category")
      )
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
}

##%######################################################%##
#                                                          #
####                       Run                          ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)

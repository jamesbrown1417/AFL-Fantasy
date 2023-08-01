# Load necessary packages
library(shiny)
library(readxl)
library(shinythemes)
library(tidyverse)
library(DT)
library(mongolite)

# Kelly criterion function------------------------------------------------------

kelly_criterion <- function(probability, odds, bankroll, half = TRUE) {
  # Calculate the edge and the odds ratio
  edge <- probability * odds - 1
  odds_ratio <- odds - 1
  
  # If the edge is less than 0, return 0 (i.e., do not bet)
  if (edge <= 0) {
    return(0)
  }
  
  # Calculate the full Kelly fraction
  kelly_fraction <- edge / odds_ratio
  
  # If half is TRUE, bet only half of the Kelly fraction
  if(half) {
    kelly_fraction <- kelly_fraction / 2
  }
  
  # Calculate the bet amount
  bet_amount <- kelly_fraction * bankroll
  
  # Return the bet amount
  return(bet_amount)
}

# Get player stats
player_stats <- readr::read_rds("../afl_fantasy_data_all.rds")

# Get player teams
player_teams <-
player_stats |>
    filter(season_name == "2023") |>
    select(player_name = player_full_name, player_team) |>
    distinct(player_name, .keep_all = TRUE) |>
    mutate(player_team = str_replace_all(player_team, "^West Coast Eagles", "West Coast")) |>
    mutate(player_team = str_replace_all(player_team, "^Geelong Cats", "Geelong")) |>
    mutate(player_team = str_replace_all(player_team, "^Gold Coast Suns", "Gold Coast")) |>
    mutate(player_team = str_replace_all(player_team, "^GWS Giants", "Greater Western Sydney")) |>
    mutate(player_team = str_replace_all(player_team, "^Adelaide Crows", "Adelaide")) |>
    mutate(player_team = str_replace_all(player_team, "^Sydney Swans", "Sydney"))


# Get DVP Data
dvp_data <- readr::read_rds("../../Data/DVP_data.rds")

# Fix names
dvp_data <-
dvp_data |>
    mutate(opposition_team = str_replace_all(opposition_team, "^West Coast Eagles", "West Coast")) |>
    mutate(opposition_team = str_replace_all(opposition_team, "^Geelong Cats", "Geelong")) |>
    mutate(opposition_team = str_replace_all(opposition_team, "^Gold Coast Suns", "Gold Coast")) |>
    mutate(opposition_team = str_replace_all(opposition_team, "^GWS Giants", "Greater Western Sydney")) |>
    mutate(opposition_team = str_replace_all(opposition_team, "^Adelaide Crows", "Adelaide")) |>
    mutate(opposition_team = str_replace_all(opposition_team, "^Sydney Swans", "Sydney"))
    
# Create fantasy and disposals df for last 10
disposals_dvp_10 <-
    dvp_data |>
    filter(n_rounds == 10 & type == "Disposals") |>
    select(opposition_team, DVP = dvp, `% Over Avg` = percentage_over_avg, Pos = pos) |>
    mutate(DVP_category = cut(DVP, breaks = quantile(DVP, probs=seq(0, 1, by=0.2), na.rm = TRUE), include.lowest = TRUE, labels = c("Terrible", "Bad", "Neutral", "Good", "Excellent")))

fantasy_dvp_10 <-
    dvp_data |>
    filter(n_rounds == 10 & type == "Fantasy Points") |>
    select(opposition_team, DVP = dvp, `% Over Avg` = percentage_over_avg, Pos = pos) |>
    mutate(DVP_category = cut(DVP, breaks = quantile(DVP, probs=seq(0, 1, by=0.2), na.rm = TRUE), include.lowest = TRUE, labels = c("Terrible", "Bad", "Neutral", "Good", "Excellent")))

# Get Player Positions
player_positions <- readr::read_rds("../../Data/afl_clustering_positions.rds")

# Get most played position to use as player's position
player_positions <-
player_positions |>
    group_by(player_name, position_name) |>
    tally() |>
    arrange(player_name, desc(n)) |>
    slice_head(n = 1) |>
    ungroup() |>
    select(player_name, position = position_name) |>
    left_join(player_teams) |>
    relocate(player_team, .after = player_name) |>
    rename(Pos = position)

# Read in datasets--------------------------------------------------------------
uri <- Sys.getenv("mongodb_connection_string")

disposals_con <- mongo(collection = "Disposals", db = "Odds", url = uri)
goals_con <- mongo(collection = "Goals",db = "Odds", url = uri)
fantasy_con <- mongo(collection = "Fantasy", db = "Odds", url = uri)
h2h_con <- mongo(collection = "H2H", db = "Odds", url = uri)

disposals <- disposals_con$find('{}') |> tibble()
goals <- goals_con$find('{}')  |> tibble()
fantasy <- fantasy_con$find('{}')  |> tibble()
h2h <- h2h_con$find('{}')  |> tibble()

# Get rid of old rounds data
disposals <- disposals |> filter(start_time > lubridate::today()) |> select(-start_time, -Season, -round)
goals <- goals |> filter(start_time > lubridate::today()) |> select(-start_time, -Season, -round)
h2h <- h2h |> filter(start_time > lubridate::today()) |> select(-Season)
fantasy <- fantasy |> filter(start_time > lubridate::today()) |> select(-start_time, -Season, -round)

# Function to get past n games player performance
get_historical_performance <- function(player_name, stat, line, n) {
    player_stats |> 
        filter(player_full_name == player_name) |> 
        arrange(desc(start_time_utc)) |> 
        slice_head(n = n) |> 
        select(season_name, round, venue, player_full_name, player_team, opposition_team, disposals:tackles, fantasy_points, `TOG %` = tog_percentage)
}

# Rename variables
disposals <-
  disposals |> 
  rename(implied_prob = implied_probability,
         emp_prob_2022 = empirical_probability_2022,
         emp_prob_2023 = empirical_probability_2023,
         emp_prob_last_3 = empirical_probability_last_3,
         emp_prob_last_5 = empirical_probability_last_5,
         emp_prob_last_7 = empirical_probability_last_7,
         emp_prob_last_10 = empirical_probability_last_10)

# Relocate
disposals <-
  disposals |> 
  relocate(emp_prob_last_7, emp_prob_last_10, .after = emp_prob_last_5) |>
  relocate(diff_last_7, diff_last_10, .after = diff_last_5)

# Get all pairwise h2h comparisons
home_h2h <-
    h2h |>
    select(start_time, round, match, home_team, home_win, home_agency = agency)

away_h2h <-
    h2h |>
    select(start_time, round, match, away_team, away_win, away_agency = agency)

h2h <-
    full_join(home_h2h, away_h2h, relationship = "many-to-many", by = c("match", "start_time", "round")) |> 
    mutate(margin = 1/home_win + 1/away_win) |> 
    mutate(margin = 100*(margin - 1)) |> 
    mutate(margin = round(margin, 2)) |>
    arrange(match, margin) |> 
    distinct(match, home_team, home_win, home_agency, away_team, away_win, away_agency, .keep_all = TRUE)

# Add number of games played to disposals, goals and fantasy
games_played <-
    player_stats |>
    filter(season_name == "2023") |>
    group_by(player_full_name, player_team) |>
    tally() |> 
    select(player_name = player_full_name, games_played = n)

disposals <- disposals |> left_join(games_played)
goals <- goals |> left_join(games_played)
fantasy <- fantasy |> left_join(games_played)

# Get discrepancies in fantasy
fantasy <-
    fantasy |> 
    arrange(player_name, fantasy_points, desc(over_price)) |> 
    group_by(match, player_name, fantasy_points) |> 
    mutate(discrepancy = max(over_implied_probability, na.rm = TRUE) - min(over_implied_probability, na.rm = TRUE)) |> 
    mutate(discrepancy = round(discrepancy, 1)) |>
    arrange(desc(discrepancy), player_name, fantasy_points)

# Get discrepancies in goals
goals <-
    goals |> 
    arrange(player_name, number_of_goals, desc(price)) |> 
    group_by(match, player_name, number_of_goals) |> 
    mutate(discrepancy = max(implied_probability, na.rm = TRUE) - min(implied_probability, na.rm = TRUE)) |> 
    mutate(discrepancy = round(discrepancy, 1)) |>
    arrange(desc(discrepancy), player_name, number_of_goals)

# Get discrepancies in disposals
disposals <-
    disposals |> 
    arrange(player_name, number_of_disposals, desc(price)) |> 
    group_by(match, player_name, number_of_disposals) |> 
    mutate(discrepancy = max(implied_prob, na.rm = TRUE) - min(implied_prob, na.rm = TRUE)) |> 
    mutate(discrepancy = round(discrepancy, 1)) |>
    arrange(desc(discrepancy), player_name, number_of_disposals)

# Add matchup Data - Disposals
disposals <-
disposals |> 
    ungroup() |>
    left_join(player_positions) |>
    separate(match, into = c("team_1", "team_2"), sep = " v ", remove = FALSE) |>
    mutate(opposition_team = ifelse(player_team == team_1, team_2, team_1)) |>
    left_join(disposals_dvp_10) |>
    select(-team_1, -team_2, -opposition_team, -player_team)

# Add matchup Data - Fantasy
fantasy <-
    fantasy |> 
    ungroup() |>
    left_join(player_positions) |>
    separate(match, into = c("team_1", "team_2"), sep = " v ", remove = FALSE) |>
    mutate(opposition_team = ifelse(player_team == team_1, team_2, team_1)) |>
    left_join(fantasy_dvp_10) |>
    select(-team_1, -team_2, -opposition_team, -player_team)

# Create an average odds column
disposals <-
disposals |> 
  group_by(match, player_name, number_of_disposals) |> 
  mutate(avg_price = mean(price)) |> 
  ungroup() |> 
  mutate(diff_vs_avg = 1/avg_price - 1/price)

# round all numeric columns to 2 decimal places
disposals <- disposals |> mutate_if(is.numeric, round, digits = 2)
h2h <- h2h |> mutate_if(is.numeric, round, digits = 2)
fantasy <- fantasy |> mutate_if(is.numeric, round, digits = 2)
goals <- goals |> mutate_if(is.numeric, round, digits = 2)
goals <- goals |> distinct(match, player_name, number_of_goals, price, agency, .keep_all = TRUE)

# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Odds Screen"),
  
  tabsetPanel(
    tabPanel(
      "Disposals",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "match",
            "Select Match:",
            choices = unique(disposals$match),
            multiple = TRUE
          ),
          selectInput(
            "agency",
            "Select Agency:",
            choices = unique(disposals$agency),
            multiple = TRUE
          ),
          textInput("player_name", "Search Player Name:", value = ""),
          selectInput(
            "num_disposals",
            "Select Number of Disposals:",
            choices = c("10+", "15+", "20+", "25+", "30+", "35+", "40+"),
            multiple = TRUE
          ),
          actionButton("reset_filters_disposals", "Reset Filters"),
          sliderInput(
            "diff_23_slider_disposals",
            label = "Select Season Differences:",
            min = min(disposals$diff_2023, na.rm = TRUE),
            max = max(disposals$diff_2023, na.rm = TRUE),
            dragRange = TRUE,
            value = c(min(disposals$diff_2023, na.rm = TRUE), max(disposals$diff_2023, na.rm = TRUE))
          ),
        sliderInput(
          "diff_last_10_slider_disposals",
          label = "Select Last 10 Game Differences:",
          min = min(disposals$diff_last_10, na.rm = TRUE),
          max = max(disposals$diff_last_10, na.rm = TRUE),
          dragRange = TRUE,
          value = c(min(disposals$diff_last_10, na.rm = TRUE), max(disposals$diff_last_10, na.rm = TRUE))
        ),
      sliderInput(
        "diff_last_7_slider_disposals",
        label = "Select Last 7 Game Differences:",
        min = min(disposals$diff_last_7, na.rm = TRUE),
        max = max(disposals$diff_last_7, na.rm = TRUE),
        dragRange = TRUE,
        value = c(min(disposals$diff_last_7, na.rm = TRUE), max(disposals$diff_last_7, na.rm = TRUE))
      ),
    sliderInput(
      "diff_last_5_slider_disposals",
      label = "Select Last 5 Game Differences:",
      min = min(disposals$diff_last_5, na.rm = TRUE),
      max = max(disposals$diff_last_5, na.rm = TRUE),
      dragRange = TRUE,
      value = c(min(disposals$diff_last_5, na.rm = TRUE), max(disposals$diff_last_5, na.rm = TRUE))
    ),
    numericInput(
      "disposals_price",
      label = "Select Max Price:",
      min = min(disposals$price, na.rm = TRUE),
      max = max(disposals$price, na.rm = TRUE),
      step = 0.01, 
      value = max(disposals$price, na.rm = TRUE)
    ),
    checkboxInput("only_best_odds", "Only Show Best Market Odds")
  ),
  mainPanel(DTOutput("disposals_table"))
)),
tabPanel("Goals",
         sidebarLayout(
           sidebarPanel(
             selectInput(
               "match_goals",
               "Select Match:",
               choices = unique(goals$match),
               multiple = TRUE
             ),
             selectInput(
               "agency_goals",
               "Select Agency:",
               choices = unique(goals$agency),
               multiple = TRUE
             ),
             textInput("player_name_goals", "Search Player Name:", value = ""),
             selectInput(
               "num_goals",
               "Select Number of Goals:",
               choices = c("1+", "2+", "3+", "4+", "5+", "6+"),
               multiple = TRUE
             ),
             actionButton("reset_filters_goals", "Reset Filters"),
             checkboxInput("only_best_odds_goals", "Only Show Best Market Odds")
           ),
           
           mainPanel(DTOutput("goals_table"))
         )),
tabPanel("Fantasy",
         sidebarLayout(
           sidebarPanel(
             selectInput(
               "match_fantasy",
               "Select Match:",
               choices = unique(fantasy$match),
               multiple = TRUE
             ),
             selectInput(
               "agency_fantasy",
               "Select Agency:",
               choices = unique(fantasy$agency),
               multiple = TRUE
             ),
             textInput("player_name_fantasy", "Search Player Name:", value = ""),
             selectInput(
               "fantasy_points",
               "Select Fantasy Points:",
               choices = unique(fantasy$fantasy_points),
               multiple = TRUE
             ),
             actionButton("reset_filters_fantasy", "Reset Filters"),
             checkboxInput("only_best_odds_fantasy", "Only Show Best Market Odds")
           ),
           
           mainPanel(DTOutput("fantasy_table"))
         )),
tabPanel("Head-to-Head",
         sidebarLayout(
           sidebarPanel(
             selectInput(
               "round_h2h",
               "Select Round:",
               choices = unique(h2h$round),
               multiple = TRUE
             ),
             selectInput(
               "match_h2h",
               "Select Match:",
               choices = unique(h2h$match),
               multiple = TRUE
             ),
             selectInput(
               "home_agency_h2h",
               "Select Home Agency:",
               choices = unique(h2h$home_agency),
               multiple = TRUE
             ),
             selectInput(
               "away_agency_h2h",
               "Select Away Agency:",
               choices = unique(h2h$away_agency),
               multiple = TRUE
             ),
             actionButton("reset_filters_h2h", "Reset Filters")
           ),
           
           mainPanel(DTOutput("h2h_table"))
         )),
tabPanel("Player History",
         sidebarLayout(
           sidebarPanel(
             textInput(inputId = "player_name_history", label = "Player Name"),
             numericInput(
               inputId = "n_games",
               label = "Number of Games",
               value = 5,
               step = 1
             ),
             actionButton("reset_filters_player_history", "Reset Filters")
           ),
           
           mainPanel(DTOutput("player_history_table"))
         ))
)
)


# Define server logic
server <- function(input, output, session) {
    filtered_data_disposals <- reactive({
        df <- disposals
        
        if (!is.null(input$match) && length(input$match) > 0) {
            df <- df %>% filter(match %in% input$match)
        }
        
        if (!is.null(input$agency) && length(input$agency) > 0) {
            df <- df %>% filter(agency %in% input$agency)
        }
        
        if (input$player_name != "") {
            df <-
                df %>% filter(grepl(input$player_name, player_name, ignore.case = TRUE))
        }
        
        if (!is.null(input$num_disposals) &&
            length(input$num_disposals) > 0) {
            df <- df %>% filter(number_of_disposals %in% input$num_disposals)
        }
        
        if (input$only_best_odds) {
          df <- df %>% filter(max_player_diff == diff_2023)
        }
        
        df <-
          df |>
          filter(diff_2023 >= input$diff_23_slider_disposals[1] & diff_2023 <= input$diff_23_slider_disposals[2]) |> 
          filter(diff_last_10 >= input$diff_last_10_slider_disposals[1] & diff_last_10 <= input$diff_last_10_slider_disposals[2]) |> 
          filter(diff_last_7 >= input$diff_last_7_slider_disposals[1] & diff_last_7 <= input$diff_last_7_slider_disposals[2]) |> 
          filter(diff_last_5 >= input$diff_last_5_slider_disposals[1] & diff_last_5 <= input$diff_last_5_slider_disposals[2]) |>
          filter(price <= input$disposals_price)
          
        
        return(df)
    })
    
    filtered_data_goals <- reactive({
        df <- goals
        
        if (!is.null(input$match_goals) && length(input$match_goals) > 0) {
            df <- df %>% filter(match %in% input$match_goals)
        }
        
        if (!is.null(input$agency_goals) && length(input$agency_goals) > 0) {
            df <- df %>% filter(agency %in% input$agency_goals)
        }
        
        if (input$player_name_goals != "") {
            df <-
                df %>% filter(grepl(input$player_name_goals, player_name, ignore.case = TRUE))
        }
        
        if (!is.null(input$num_goals) &&
            length(input$num_goals) > 0) {
            df <- df %>% filter(number_of_goals %in% input$num_goals)
        }
        
        if (input$only_best_odds_goals) {
          df <- df %>% filter(max_player_diff == diff_2023)
        }
        
        return(df)
    })
    
    filtered_data_fantasy <- reactive({
        df <- fantasy
        
        if (!is.null(input$match_fantasy) &&
            length(input$match_fantasy) > 0) {
            df <- df %>% filter(match %in% input$match_fantasy)
        }
        
        if (!is.null(input$agency_fantasy) &&
            length(input$agency_fantasy) > 0) {
            df <- df %>% filter(agency %in% input$agency_fantasy)
        }
        
        if (input$player_name_fantasy != "") {
            df <-
                df %>% filter(grepl(
                    input$player_name_fantasy,
                    player_name,
                    ignore.case = TRUE
                ))
        }
        
        if (!is.null(input$fantasy_points) &&
            length(input$fantasy_points) > 0) {
            df <- df %>% filter(fantasy_points %in% input$fantasy_points)
        }
        
        if (input$only_best_odds_fantasy) {
          df <- df %>% filter(max_player_diff == diff_2023)
        }
        
        return(df)
    })
    
    filtered_data_h2h <- reactive({
        df <- h2h
        
        if (!is.null(input$round_h2h) &&
            length(input$round_h2h) > 0) {
            df <- df %>% filter(round %in% input$round_h2h)
        }
        
        if (!is.null(input$match_h2h) &&
            length(input$match_h2h) > 0) {
            df <- df %>% filter(match %in% input$match_h2h)
        }
        
        if (!is.null(input$home_agency_h2h) &&
            length(input$home_agency_h2h) > 0) {
            df <- df %>% filter(home_agency %in% input$home_agency_h2h)
        }
        
        if (!is.null(input$away_agency_h2h) &&
            length(input$away_agency_h2h) > 0) {
            df <- df %>% filter(away_agency %in% input$away_agency_h2h)
        }
        
        return(df)
    })
    
    # Player History
    filtered_data_player_history <- reactive({
        if (input$player_name_history == "") {
            return(NULL)
        }
        df <- get_historical_performance(player_name = input$player_name_history, stat = "disposals", line = "number_of_disposals", n = input$n_games)
        return(df)
    })
    

    output$disposals_table <- renderDT({
        datatable(filtered_data_disposals(),
                  filter = "top",
                  options = list(
                      pageLength = 25,
                      autoWidth = FALSE,
                      lengthMenu = c(10, 25, 50, 100)
                  ))
    })
    
    output$goals_table <- renderDT({
        datatable(filtered_data_goals(),
                  filter = "top",
                  options = list(
                      pageLength = 25,
                      lengthMenu = c(10, 25, 50, 100)
                  ))
    })
    
    output$fantasy_table <- renderDT({
        datatable(filtered_data_fantasy(),
                  filter = "top",
                  options = list(
                      pageLength = 25,
                      lengthMenu = c(10, 25, 50, 100)
                  ))
    })
    
    output$h2h_table <- renderDT({
        datatable(filtered_data_h2h(),
                  filter = "top",
                  options = list(
                      pageLength = 25,
                      lengthMenu = c(10, 25, 50, 100)
                  ))
    })
    
    output$player_history_table <- renderDT({
        data <- filtered_data_player_history()
        if (is.null(data)) {
            return(NULL)
        }
        datatable(data,filter = "top",
                  options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100)))
    })
    
    observeEvent(input$reset_filters_disposals, {
        updateSelectInput(session, "match", selected = NULL)
        updateSelectInput(session, "agency", selected = NULL)
        updateTextInput(session, "player_name", value = "")
        updateSelectInput(session, "num_disposals", selected = NULL)
    })
    
    observeEvent(input$reset_filters_goals, {
        updateSelectInput(session, "match_goals", selected = NULL)
        updateSelectInput(session, "agency_goals", selected = NULL)
        updateTextInput(session, "player_name_goals", value = "")
        updateSelectInput(session, "num_goals", selected = NULL)
    })
    
    observeEvent(input$reset_filters_fantasy, {
        updateSelectInput(session, "match_fantasy", selected = NULL)
        updateSelectInput(session, "agency_fantasy", selected = NULL)
        updateTextInput(session, "player_name_fantasy", value = "")
        updateSelectInput(session, "fantasy_points", selected = NULL)
    })
    
    observeEvent(input$reset_filters_h2h, {
        updateTextInput(session, "player_name", value = "")
        updateNumericInput(session, "n", value = 5)
    })
    
    # Player History reset filters observer
    observeEvent(input$reset_filters_player_history, {
        updateTextInput(session, "player_name", value = "")
        updateNumericInput(session, "n", value = 5)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

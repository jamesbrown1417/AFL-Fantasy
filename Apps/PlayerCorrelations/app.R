# Libraries and data
library(tidyverse)
library(shiny)
library(shinythemes)

# Get player stats
player_stats <- readr::read_rds("../afl_fantasy_data_all.rds")

# Function to get player correlations
get_player_correlations <- function(player_a, player_b, line_a, line_b, type, seasons = 2014:2023) {
  # Individual DFs
  a_data <- player_stats |>
    filter(player_full_name == player_a) |> 
    select(season_name, round, player_team, player_a_stat = type) |> 
    mutate(season_name = as.numeric(season_name))
  
  b_data <- player_stats |>
    filter(player_full_name == player_b) |> 
    select(season_name, round, player_team, player_b_stat = type) |> 
    mutate(season_name = as.numeric(season_name))
  
  # Get games where players played together
  combined_data <- inner_join(a_data, b_data) |> filter(season_name %in% seasons)
  
  # Get games where player A covered line
  player_a_covered <- combined_data |> filter(player_a_stat >= line_a)
  
  # Get games where player B covered line
  player_b_covered <- combined_data |> filter(player_b_stat >= line_b)
  
  # Get unconditional probabilities
  player_a_prob <- mean(combined_data$player_a_stat >= line_a)
  player_b_prob <- mean(combined_data$player_b_stat >= line_b)
  
  # Get conditional probabilities
  player_a_cond_prob <- mean(player_b_covered$player_a_stat >= line_a)
  player_b_cond_prob <- mean(player_a_covered$player_b_stat >= line_b)
  
  # Create table
  table <-
    tibble(
      player = c(player_a, player_b),
      n = nrow(combined_data),
      stat_line = c(line_a, line_b),
      probability = c(player_a_prob, player_b_prob),
      conditional_probability = c(player_a_cond_prob, player_b_cond_prob)
    )
  
  # Perform a fisher exact test
  new_dat <-
    combined_data |> 
    mutate(player_a_success = player_a_stat >= line_a,
           player_b_success = player_b_stat >= line_b)
  
  test <- fisher.test(new_dat$player_a_success, new_dat$player_b_success)
  
  # Calculate estimated multi price
  est_price_1 = 1 / (table[[1, 4]] * table[[2, 5]])
  est_price_2 = 1 / (table[[2, 4]] * table[[1, 5]])
  
  # Add to table
  table$est_price = 1 / table$probability
  table$est_multi_price = c(est_price_1, est_price_2)
  table$p_value = test$p.value
  
  # Output
  table
}

# Function to plot player stats against each other
plot_corr <- function(player_a, player_b, type, seasons = 2014:2023, a_line, b_line) {
  # Individual DFs
  a_data <- player_stats |>
    filter(player_full_name == player_a) |> 
    select(season_name, round, player_team, player_a_stat = type) |> 
    mutate(season_name = as.numeric(season_name))
  
  b_data <- player_stats |>
    filter(player_full_name == player_b) |> 
    select(season_name, round, player_team, player_b_stat = type) |> 
    mutate(season_name = as.numeric(season_name))
  
  # Get games where players played together
  combined_data <- inner_join(a_data, b_data) |> filter(season_name %in% seasons)
  
  # Plot
  combined_data |> 
    ggplot(aes(x = player_a_stat, y = player_b_stat)) +
    geom_jitter(alpha = 0.6, size = 3, color = "darkblue", width = 0.05, height = 0.05) +
    geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "red") +
    geom_vline(xintercept = a_line, linetype = "dashed") +
    geom_hline(yintercept = b_line, linetype = "dashed") +
    labs(
      x = paste0("Player A (", player_a, ") ", type),
      y = paste0("Player B (", player_b, ") ", type),
      title = paste0("Correlation of ", type, " between ", player_a, " and ", player_b),
      caption = "Data source: Player Stats"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      plot.caption = element_text(size = 10, hjust = 1)
    )
}

# Shiny app UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Player Correlations"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("player_a", "Player A Name:", ""),
      numericInput("line_a", "Stat Line for Player A:", value = 0, min = 0),
      textInput("player_b", "Player B Name:", ""),
      numericInput("line_b", "Stat Line for Player B:", value = 0, min = 0),
      selectInput("type", "Stat Type:", choices = c("fantasy_points", "disposals", "tackles", "marks", "goals")),
      selectizeInput("seasons", "Seasons:", choices = 2014:2023, multiple = TRUE, selected = 2023)
    ),
    
    mainPanel(
      tableOutput("correlation_table"),
      plotOutput("correlation_plot")
    )
  )
)

# Shiny app server
server <- function(input, output) {
  output$correlation_table <- renderTable({
    req(input$player_a, input$player_b, input$line_a, input$line_b, input$type, input$seasons)
    
    selected_seasons <- as.numeric(unlist(input$seasons))
    
    get_player_correlations(
      player_a = input$player_a,
      player_b = input$player_b,
      line_a = input$line_a,
      line_b = input$line_b,
      type = input$type,
      seasons = selected_seasons
    )
  })
  
  output$correlation_plot <- renderPlot({
    req(input$player_a, input$player_b, input$type, input$seasons)
    
    selected_seasons <- as.numeric(unlist(input$seasons))
    
    plot_corr(
      player_a = input$player_a,
      player_b = input$player_b,
      type = input$type,
      seasons = selected_seasons,
      a_line = input$line_a,
      b_line = input$line_b
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
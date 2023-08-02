#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(mongolite)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Create compare sgm function
#===============================================================================

# Source scripts
source("betright_sgm.R")
source("tab_sgm.R")
source("sportsbet_sgm.R")
source("pointsbet_sgm.R")
source("palmerbet_sgm.R")

# Create compare sgm function
compare_sgm <- function(player_names, disposal_counts) {
  # Function to handle errors in the call_sgm functions
  handle_call_sgm <- function(func, sgm, player_names, disposal_counts) {
    tryCatch({
      func(sgm, player_names, disposal_counts)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections=NA, Unadjusted_Price=NA, Adjusted_Price=NA, Adjustment_Factor=NA, Agency=NA)
    })
  }
  
  # Get individual dataframes
  pointsbet_data <- handle_call_sgm(call_sgm_pointsbet, pointsbet_sgm, player_names, disposal_counts)
  sportsbet_data <- handle_call_sgm(call_sgm_sportsbet, sportsbet_sgm, player_names, disposal_counts)
  tab_data <- handle_call_sgm(call_sgm_tab, tab_sgm, player_names, disposal_counts)
  betright_data <- handle_call_sgm(call_sgm_betright, betright_sgm, player_names, disposal_counts)
  palmerbet_data <- handle_call_sgm(call_sgm_palmerbet, palmerbet_sgm, player_names, disposal_counts)
  
  # Bind together and return
  bind_rows(pointsbet_data, sportsbet_data, tab_data, betright_data, palmerbet_data) |>
    mutate(Adjusted_Price = round(Adjusted_Price, 2),
           Unadjusted_Price = round(Unadjusted_Price, 2),
           Adjustment_Factor = round(Adjustment_Factor, 2)
           ) |>
    arrange(desc(Adjusted_Price))
}

# Read in datasets--------------------------------------------------------------
uri <- Sys.getenv("mongodb_connection_string")

disposals_con <- mongo(collection = "Disposals", db = "Odds", url = uri)
disposals <- disposals_con$find('{}') |> tibble()

# Unique matches
matches <-
  disposals |>
  arrange(start_time) |> 
  filter(start_time > now()) |> 
  distinct(match) |>
  pull()
  
# Unique agencies
agencies <-
  disposals |>
  distinct(agency) |>
  pull()

# Create disposals dataframe to display
disposals_display <-
  disposals |>
  arrange(desc(max_player_diff)) |> 
  transmute(match,
         player_name,
         number_of_disposals,
         price,
         agency,
         prob_2023 = round(empirical_probability_2023, 2),
         prob_last_7 = round(empirical_probability_last_7, 2),
         diff_2023 = round(diff_2023, 2),
         diff_last_7 = round(diff_last_7, 2))

# Get correlations
correlations_2023 <-
  read_rds("../../Data/player_correlations_disposals_23.rds") |> 
  mutate_if(is.numeric, round, digits = 2)

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("match", "Select Match", choices = matches, selected = NULL),
      selectInput("agency", "Select Agency", choices = agencies, selected = NULL),
      h3("Selections"),  # Header in sidebar
      DTOutput("selected"),  # DataTable in sidebar
      h3("Pairwise Correlations"),
      DTOutput(outputId = "correlations"),
      h3("SGM Information"),
      uiOutput(outputId = "summary"),
      h3("Odds Comparison"),
      actionButton("get_comparison", label = "Compare Odds"),
      DTOutput(outputId = "odds_compare")
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output, session) {
  
  output$table <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    datatable(filtered_data, selection = "multiple")
  }, server = FALSE) # We are setting this as FALSE for client-side processing of the DataTable
  
  observeEvent(input$table_rows_selected,{
    output$selected <- renderDT({
      if(!is.null(input$table_rows_selected)){
        filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
        selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
        datatable(selected_data)
      }
    })
  })
  
  output$correlations <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
    
    correlations_table <- correlations_2023 |> filter(player_a %in% selected_data$player_name & player_b %in% selected_data$player_name)
    datatable(correlations_table)
  })
  
  observeEvent(input$get_comparison, {
    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
    
    player_names = selected_data$player_name
    number_of_disposals = selected_data$number_of_disposals
    
    # Call function
    comparison_df <- compare_sgm(player_names, number_of_disposals)
    
    # populate DTOutput
    output$odds_compare <- renderDT({
      datatable(comparison_df)
    })
  })

  output$summary <- renderUI({
    if(!is.null(input$table_rows_selected)){
      filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
      selected_data <- filtered_data[input$table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_2023)
      HTML(paste0("<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
                 " <strong>Empirical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)))
    }
  })
}

##%######################################################%##
#                                                          #
####                      Run App                       ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)

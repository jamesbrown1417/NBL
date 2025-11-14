library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(readr)
library(lubridate)

# Function to convert time to decimal-------------------------------------------
convert_time_to_decimal_hms <- function(time_obj) {
  # Convert to hms object
  time_obj <- hms(time_obj)
  
  # Extract hours and minutes
  hours <- hour(time_obj)
  minutes <- minute(time_obj)
  
  # Convert to decimal
  decimal_time <- hours + (minutes / 60)
  return(decimal_time)
}

# Function to get correlation between players-----------------------------------
get_player_correlation <- function(seasons = NULL, name_a, name_b, metric_a, metric_b) {
  # Column names for later use
  col_name_a <- paste0(name_a, " ", metric_a)
  col_name_b <- paste0(name_b, " ", metric_b)
  
  # Get dataframe for player A
  df_player_a <- 
    all_player_stats %>%
    filter(PLAYER_NAME == name_a & SEASON_YEAR %in% seasons) |> 
    select(match_id, PLAYER_NAME, all_of(metric_a)) |> 
    rename(!!col_name_a := all_of(metric_a))
  
  # Get dataframe for player B
  df_player_b <- 
    all_player_stats %>%
    filter(PLAYER_NAME == name_b & SEASON_YEAR %in% seasons) |> 
    select(match_id, PLAYER_NAME, all_of(metric_b)) |> 
    rename(!!col_name_b := all_of(metric_b))
  
  # Merge the two dataframes
  df_merged <- inner_join(df_player_a, df_player_b, by = "match_id")
  
  # Compute correlation
  correlation <- cor(df_merged[[col_name_a]], df_merged[[col_name_b]], method = "pearson")
  cat(sprintf("The correlation between %s and %s is: %f\n", col_name_a, col_name_b, correlation))
  
  # Create plot
  ggplot(df_merged, aes(x = .data[[col_name_a]], y = .data[[col_name_b]])) +
    geom_point(color = "#3498db", alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dashed") +
    labs(
      x = col_name_a, 
      y = col_name_b,
      title = "Player Performance Correlation",
      subtitle = sprintf("Correlation between %s and %s", col_name_a, col_name_b),
      caption = sprintf("Pearson's r: %.2f", correlation)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
      plot.caption = element_text(hjust = 1, color = "grey50"),
      text = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    ) +
    annotate(
      "text", x = max(df_merged[[col_name_a]]), y = min(df_merged[[col_name_b]]), 
      label = sprintf("r = %.2f", correlation), 
      hjust = 1, vjust = 0, size = 5, color = "red1", fontface = "italic"
    )
}

# Function to compare player performance w or w/o teammate----------------------
compare_performance <- function(seasons = NULL, main_name, teammate_name, metric) {
  # Filter the data for games with the main player
  df_player <-
    all_player_stats %>%
    filter(PLAYER_NAME == main_name) %>%
    filter(SEASON_YEAR %in% seasons)
  
  # Find the game IDs where the teammate also played
  games_with_teammate <-
    all_player_stats %>%
    filter(SEASON_YEAR %in% seasons) %>%
    filter(PLAYER_NAME == teammate_name) %>% pull(match_id)
  
  # Label each game as 'With Teammate' or 'Without Teammate'
  df_player <- df_player %>% 
    mutate(Teammate = if_else(match_id %in% games_with_teammate, 'With Teammate', 'Without Teammate'))
  
  # Calculate mean and count for both conditions
  summary_stats <- df_player %>% group_by(Teammate) %>% summarise(mean_val = mean(!!sym(metric), na.rm = TRUE), n_games = n())
  
  # Create the violin plot
  plot <- ggplot(df_player, aes(x = Teammate, y = !!sym(metric), fill = Teammate)) +
    geom_violin(trim = FALSE, position = position_dodge(width = 0.9)) +
    geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
    labs(title = paste("Performance of", main_name, "with and without", teammate_name),
         x = "Condition",
         y = metric) +
    scale_fill_manual(values = c("Without Teammate" = "orange1", "With Teammate" = "royalblue1")) +
    annotate("text", x = Inf, y = Inf, 
             label = paste("With Teammate: ", summary_stats$n_games[summary_stats$Teammate == "With Teammate"], 
                           " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "With Teammate"], 2), "\n",
                           "Without Teammate: ", summary_stats$n_games[summary_stats$Teammate == "Without Teammate"], 
                           " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "Without Teammate"], 2)), 
             hjust = 1, vjust = 1) +
    theme_minimal()
  
  return(plot)
}

#===============================================================================
# Read in Data
#===============================================================================

all_player_stats <-
  read_rds("../../Data/combined_stats_table.rds") |> 
  mutate(PLAYER_NAME = paste(first_name, family_name)) |>
  mutate(minutes_played = ifelse(str_detect(player_minutes, "\\:"), period_to_seconds(ms(player_minutes)) / 60, player_minutes)) |>
  mutate(minutes_played = as.numeric(minutes_played)) |>
  rename(
         PTS = player_points,
         REB = player_rebounds_total,
         AST = player_assists,
         STL = player_steals,
         BLK = player_blocks,
         MIN = minutes_played,
         SEASON_YEAR = season) |> 
  mutate(MIN = round(MIN, 2)) |> 
  mutate(PRA = PTS + REB + AST) |> 
  mutate(HOME_TEAM = ifelse(home_away == "home", name, opp_name)) |>
  mutate(AWAY_TEAM = ifelse(home_away == "away", name, opp_name))

# DVP RDS (if available) -------------------------------------------------------
dvp_results <- tryCatch(
  read_rds("../../Data/processed_stats/dvp_results.rds"),
  error = function(e) NULL
)

dvp_available <- !is.null(dvp_results)

if (dvp_available) {
  dvp_points   <- dvp_results$points
  dvp_rebounds <- dvp_results$rebounds
  dvp_assists  <- dvp_results$assists
  dvp_threes   <- dvp_results$threes
  dvp_steals   <- dvp_results$steals
  dvp_blocks   <- dvp_results$blocks
  dvp_pras     <- dvp_results$pras
} else {
  dvp_points   <- tibble(position = character(), opposition = character(), games = integer(), avg_points = numeric())
  dvp_rebounds <- tibble(position = character(), opposition = character(), games = integer(), avg_rebounds = numeric())
  dvp_assists  <- tibble(position = character(), opposition = character(), games = integer(), avg_assists = numeric())
  dvp_threes   <- tibble(position = character(), opposition = character(), games = integer(), avg_threes = numeric())
  dvp_steals   <- tibble(position = character(), opposition = character(), games = integer(), avg_steals = numeric())
  dvp_blocks   <- tibble(position = character(), opposition = character(), games = integer(), avg_blocks = numeric())
  dvp_pras     <- tibble(position = character(), opposition = character(), games = integer(), avg_pras = numeric())
}

# Conditional logic for if operating system is windows
# Read Odds Data----------------------------------------------------------------
player_points_data  <- read_rds("../../Data/processed_odds/all_player_points.rds")
player_assists_data <- read_rds("../../Data/processed_odds/all_player_assists.rds")
player_rebounds_data<- read_rds("../../Data/processed_odds/all_player_rebounds.rds")
player_threes_data  <- read_rds("../../Data/processed_odds/all_player_threes.rds")

# New markets
player_pras_data    <- tryCatch(read_rds("../../Data/processed_odds/all_player_pras.rds"), error = function(e) tibble())
player_steals_data  <- tryCatch(read_rds("../../Data/processed_odds/all_player_steals.rds"), error = function(e) tibble())
player_blocks_data  <- tryCatch(read_rds("../../Data/processed_odds/all_player_blocks.rds"), error = function(e) tibble())

# Aggregate choices
all_agencies <- unique(c(player_points_data$agency, player_assists_data$agency, player_rebounds_data$agency, player_threes_data$agency,
                         player_pras_data$agency, player_steals_data$agency, player_blocks_data$agency))
all_agencies <- all_agencies[!is.na(all_agencies)]
all_matches  <- unique(c(player_points_data$match, player_assists_data$match, player_rebounds_data$match, player_threes_data$match,
                         player_pras_data$match, player_steals_data$match, player_blocks_data$match))
all_matches  <- all_matches[!is.na(all_matches)]

# # Add opposition defensive rating-----------------------------------------------
# 
# # Get defensive rating in last 5 games
# def_rating_last_5 <-
#   all_team_stats |> 
#   arrange(teamName, desc(date)) |>
#   group_by(teamName) |>
#   slice_head(n = 5) |>
#   summarise(def_rating = mean(defensiveRating, na.rm = TRUE)) |> 
#   mutate(def_rating = round((def_rating - min(def_rating)) / (max(def_rating) - min(def_rating)) * 100, digits = 1))
# 
# # Get Pace per 40 vs opposition in last 5 games
# pace_per_40_last_5 <-
#   all_team_stats |> 
#   arrange(oppositionTeam, desc(date)) |>
#   group_by(oppositionTeam) |>
#   slice_head(n = 5) |>
#   summarise(pace_per_40 = mean(pacePer40, na.rm = TRUE)) |> 
#   mutate(pace_per_40 = round((pace_per_40 - min(pace_per_40)) / (max(pace_per_40) - min(pace_per_40)) * 100, digits = 1))
# 
# # Add to player points
# player_points_data <-
#   player_points_data |> 
#   left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
#   left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))
# 
# # Add to player assists
# player_assists_data <-
#   player_assists_data |> 
#   left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
#   left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))
# 
# # Add to player rebounds
# player_rebounds_data <-
#   player_rebounds_data |> 
#   left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
#   left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

#===============================================================================
# UI
#===============================================================================

ui <- page_navbar(
  title = "NBL",
  selected = "Player Stats",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  tags$head(
    tags$style(HTML("
      .tab-content, .tab-pane {
        height: 1250px;
        overflow-y: auto;
      }
      .dataTables_wrapper {
        overflow-x: auto;
      }
    "))
  ),
  nav_panel(
    title = "Player Stats",
    grid_container(
      layout = c("nbl_stats player_stat_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "nbl_stats",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "player_name_input_a",
            label = "Select Player:",
            selected = "Bryce Cotton",
            choices = all_player_stats$PLAYER_NAME |> unique(),
          ),
          selectInput(
            inputId = "season_input_a",
            label = "Select Season:",
            choices = all_player_stats$SEASON_YEAR |> unique(),
            multiple = TRUE,
            selectize = TRUE,
            selected = intersect(
              c(
                "2021-2022",
                "2022-2023",
                "2023-2024",
                "2024-2025",
                "2025-2026"
              ),
              all_player_stats$SEASON_YEAR |> unique()
            )
          ),
          selectInput(
            inputId = "stat_input_a",
            label = "Select Statistic:",
            choices = c("PTS",
                        "REB",
                        "AST",
                        "PRA",
                        "FG3M",
                        "BLK",
                        "STL",
                        "MIN"),
            multiple = FALSE,
            selected = "PTS"
          ),
          radioButtons(
            inputId = "probability_mode_a",
            label = "Probability Mode",
            choices = c("Over/Under", "Interval"),
            selected = "Over/Under"
          ),
          checkboxGroupInput(
            inputId = "home_status",
            label = "Home / Away Games",
            choices = list("Home" = "home", "Away" = "away"),
            selected = c("home", "away")
          ),
          markdown(mds = c("__Select Only Last n Games:__")),
          numericInput(
            inputId = "last_games",
            label = "Number of Games",
            value = NA
          ),
          markdown(mds = c("__Reference or Interval:__")),
          conditionalPanel(
            condition = "input.probability_mode_a == 'Over/Under'",
            numericInput(
              inputId = "reference_line",
              label = "Line Value",
              value = 19.5
            )
          ),
          conditionalPanel(
            condition = "input.probability_mode_a == 'Interval'",
            numericInput(
              inputId = "interval_lower_a",
              label = "Lower Bound",
              value = 15.5
            ),
            numericInput(
              inputId = "interval_upper_a",
              label = "Upper Bound",
              value = 25.5
            )
          ),
          checkboxInput(
            inputId = "enable_second_stat",
            label = "Add Second Stat",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.enable_second_stat",
            selectInput(
              inputId = "stat_input_b",
              label = "Second Statistic:",
              choices = c("PTS", "REB", "AST", "PRA", "FG3M", "BLK", "STL", "MIN"),
              multiple = FALSE,
              selected = "AST"
            ),
            radioButtons(
              inputId = "probability_mode_b",
              label = "Second Stat Mode",
              choices = c("Over/Under", "Interval"),
              selected = "Over/Under"
            ),
            conditionalPanel(
              condition = "input.probability_mode_b == 'Over/Under'",
              numericInput(
                inputId = "reference_line_b",
                label = "Second Line Value",
                value = 4.5
              )
            ),
            conditionalPanel(
              condition = "input.probability_mode_b == 'Interval'",
              numericInput(
                inputId = "interval_lower_b",
                label = "Second Lower Bound",
                value = 3.5
              ),
              numericInput(
                inputId = "interval_upper_b",
                label = "Second Upper Bound",
                value = 8.5
              )
            )
          ),
          markdown(mds = c("__Select Minutes Range:__")),
          numericInput(
            inputId = "minutes_minimum",
            label = "Min Minutes",
            value = 0
          ),
          numericInput(
            inputId = "minutes_maximum",
            label = "Max Minutes",
            value = 60
          )
        )
      ),
      grid_card(area = "player_stat_plot",
                card_body(
                  tabsetPanel(
                    id = "stat_tabs",
                    tabPanel(
                      "Plot",
                      uiOutput(outputId = "plots_container")
                    ),
                    tabPanel(
                      "Table",
                      DTOutput(
                        outputId = "player_stat_table",
                        width = "100%",
                        height = "800px"
                      )
                    )
                  )
                ))
    )
  ),
  nav_panel(title = "Odds Screen",
            grid_container(
              layout = c("odds_screen odds_table"),
              row_sizes = c("1fr"),
              col_sizes = c("250px",
                            "1fr"),
              gap_size = "10px",
              grid_card(area = "odds_screen",
                        card_header("Settings"),
                        card_body(
                          selectInput(
                            inputId = "agency_input",
                            label = "Select Agencies:",
                            choices = all_agencies,
                            multiple = TRUE,
                            selectize = TRUE,
                            selected = all_agencies,
                          ),
                          selectInput(
                            inputId = "market_input",
                            label = "Select Market:",
                            choices = c("Points", "Rebounds", "Assists", "Threes", "PRAs", "Steals", "Blocks"),
                            multiple = FALSE
                          ),
                          selectInput(
                            inputId = "match_input",
                            label = "Select Matches:",
                            choices = all_matches,
                            multiple = TRUE,
                            selectize = FALSE,
                            selected = all_matches
                          ),
                          textInput(
                            inputId = "player_name_input_b",
                            label = "Select Player:",
                            value = NA
                          ),
                          checkboxInput(
                            inputId = "only_unders",
                            label = "Only Show Markets With Unders",
                            value = FALSE
                          ),
                          checkboxInput(
                            inputId = "only_best",
                            label = "Only Show Best Market Odds",
                            value = FALSE
                          ),
                          markdown(mds = c("__Select Odds Range:__")),
                          numericInput(
                            inputId = "odds_minimum",
                            label = "Min Odds",
                            value = NA
                          ),
                          numericInput(
                            inputId = "odds_maximum",
                            label = "Max Odds",
                            value = NA
                          ),
                          markdown(mds = c("__Select Difference Range 2024:__")),
                          numericInput(
                            inputId = "diff_minimum_24",
                            label = "Min Diff",
                            value = NA
                          ),
                          numericInput(
                            inputId = "diff_maximum_24",
                            label = "Max Diff",
                            value = NA
                          ),
                          markdown(mds = c("__Select Difference Range 2023:__")),
                          numericInput(
                            inputId = "diff_minimum_23",
                            label = "Min Diff",
                            value = NA
                          ),
                          numericInput(
                            inputId = "diff_maximum_23",
                            label = "Max Diff",
                            value = NA
                          )
                        )),
              grid_card(area = "odds_table",
                        card_body(
                          DTOutput(outputId = "scraped_odds_table", height = "1500px")
                        ))
            )),
  nav_panel(
    title = "DVP",
    grid_container(
      layout = c("dvp_controls dvp_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("300px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "dvp_controls",
        card_header("Settings"),
        card_body(
          if (!dvp_available) {
            div(
              style = "color:#b22222;",
              "DVP data not found. Run Scripts/06-defence-vs-position.R to generate RDS."
            )
          },
          selectInput(
            inputId = "dvp_stat",
            label = "Statistic",
            choices = c("Points", "Rebounds", "Assists", "Threes", "Steals", "Blocks", "PRAs"),
            selected = "Points"
          ),
          numericInput(
            inputId = "dvp_min_games",
            label = "Min players per cell",
            value = 3, min = 1, step = 1
          ),
          checkboxInput(
            inputId = "dvp_show_labels",
            label = "Show labels",
            value = TRUE
          )
        )
      ),
      grid_card(
        area = "dvp_plot",
        card_body(
          plotOutput("dvp_heatmap", height = "900px")
        )
      )
    )
  ),
  nav_panel(
    title = "With / Without Teammate",
    grid_container(
      layout = c("with_without_settings with_without_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("500px", "1fr"),
      gap_size = "10px",
      
      grid_card(
        area = "with_without_settings",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "player_name",
            label = "Select Player:",
            selected = "Bryce Cotton",
            choices = all_player_stats$PLAYER_NAME |> unique()
          ),
          selectInput(
            inputId = "teammate_name",
            label = "Select Teammate:",
            selected = "Keanu Pinder",
            choices = all_player_stats$PLAYER_NAME |> unique()
          ),
          selectInput(
            inputId = "season_input",
            label = "Select Season:",
            choices = all_player_stats$SEASON_YEAR |> unique(),
            multiple = TRUE,
            selectize = TRUE,
            selected = intersect(
              c(
                "2021-2022",
                "2022-2023",
                "2023-2024",
                "2024-2025",
                "2025-2026"
              ),
              all_player_stats$SEASON_YEAR |> unique()
            )
          ),
          selectInput(
            inputId = "metric_input",
            label = "Select Statistic:",
            choices = c("PTS", "REB", "AST", "BLK", "MIN"),
            multiple = FALSE,
            selected = "PTS"
          )
        )
      ),
      
      grid_card(area = "with_without_plot",
                card_body(
                  plotOutput(outputId = "with_without_plot_output", height = "800px", width = "50%")
                ))
    )
  ),
  nav_panel(
    title = "Player Correlations",
    grid_container(
      layout = c("corr_settings corr_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("500px", "1fr"),
      gap_size = "10px",
      
      grid_card(
        area = "corr_settings",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "player_name_corr",
            label = "Select Player 1:",
            selected = "Bryce Cotton",
            choices = all_player_stats$PLAYER_NAME |> unique()
          ),
          selectInput(
            inputId = "metric_input_corr_a",
            label = "Select Statistic:",
            choices = c("PTS", "REB", "AST", "BLK", "STL", "MIN"),
            multiple = FALSE,
            selected = "PTS"
          ),
          selectInput(
            inputId = "teammate_name_corr",
            label = "Select Player 2:",
            selected = "Keanu Pinder",
            choices = all_player_stats$PLAYER_NAME |> unique()
          ),
          selectInput(
            inputId = "metric_input_corr_b",
            label = "Select Statistic:",
            choices = c("PTS", "REB", "AST", "BLK", "STL", "MIN"),
            multiple = FALSE,
            selected = "PTS"
          ),
          selectInput(
            inputId = "season_input_corr",
            label = "Select Season:",
            choices = all_player_stats$SEASON_YEAR |> unique(),
            multiple = TRUE,
            selectize = TRUE,
            selected = intersect(
              c(
                "2021-2022",
                "2022-2023",
                "2023-2024",
                "2024-2025",
                "2025-2026"
              ),
              all_player_stats$SEASON_YEAR |> unique()
            )
          )
        )
      ),
      
      grid_card(area = "corr_plot",
                card_body(
                  plotOutput(outputId = "corr_plot_output", height = "800px", width = "50%")
                ))
    )
  )
)


#===============================================================================
# Server
#===============================================================================

server <- function(input, output) {
  #=============================================================================
  # Filter player stats
  #=============================================================================
  
  filtered_player_stats <- reactive({
    # Filter player stats
    filtered_player_stats <-
      all_player_stats |>
      filter(
        PLAYER_NAME == input$player_name_input_a,
        SEASON_YEAR %in% input$season_input_a,
        MIN >= input$minutes_minimum,
        MIN <= input$minutes_maximum,
        home_away %in% input$home_status
      ) |>
      arrange(match_time_utc) |>
      mutate(game_number = row_number()) |> 
      select(Date = match_time_utc,
             Home = HOME_TEAM,
             Away = AWAY_TEAM,
             Player = PLAYER_NAME,
             Team = name,
             MIN,
             FGM = player_field_goals_made,
             FGA = player_field_goals_attempted,
             FG_PCT = player_field_goals_percentage,
             FG3M = player_three_pointers_made,
             FG3A = player_three_pointers_attempted,
             FG3_PCT = player_three_pointers_percentage,
             FTM = player_free_throws_made,
             FTA = player_free_throws_attempted,
             FT_PCT = player_free_throws_percentage,
             PTS,
             REB,
             AST,
             PRA,
             BLK,
             STL,
             game_number) |> 
      arrange(desc(Date))
             
    # Filter by last n games
    if (!is.na(input$last_games)) {
      filtered_player_stats <-
        filtered_player_stats |>
        slice_head(n = input$last_games)
    }
    
    # Return filtered player stats
    return(filtered_player_stats)
    
  })
  
  #=============================================================================
  # Get Proportion above reference line
  #=============================================================================
  
  probability_summary <- reactive({
    df <- filtered_player_stats()
    n <- nrow(df)
    if (n == 0) return("No games after filters")

    # Condition A
    vals_a <- df[[input$stat_input_a]]
    if (is.null(vals_a)) return("Invalid primary stat selection")

    if (identical(input$probability_mode_a, "Interval")) {
      lower_a <- input$interval_lower_a
      upper_a <- input$interval_upper_a
      cond_a <- !is.na(vals_a) & vals_a >= lower_a & vals_a <= upper_a
      label_a <- paste0(input$stat_input_a, " in [", lower_a, ", ", upper_a, "]")
    } else {
      ref_a <- input$reference_line
      cond_a <- !is.na(vals_a) & vals_a >= ref_a
      label_a <- paste0(input$stat_input_a, " >= ", ref_a)
    }
    p_a <- mean(cond_a, na.rm = TRUE)

    # Build summary
    parts <- c(
      paste0("P(", label_a, ") = ", round(p_a, 2),
             " | Over Odds: ", ifelse(is.finite(1/p_a), round(1/p_a, 2), "Inf"),
             ", Under Odds: ", ifelse(is.finite(1/(1-p_a)), round(1/(1-p_a), 2), "Inf")),
      paste0("Sample Size: ", n)
    )

    # Optional second stat
    if (isTRUE(input$enable_second_stat)) {
      vals_b <- df[[input$stat_input_b]]
      if (!is.null(vals_b)) {
        if (identical(input$probability_mode_b, "Interval")) {
          lower_b <- input$interval_lower_b
          upper_b <- input$interval_upper_b
          cond_b <- !is.na(vals_b) & vals_b >= lower_b & vals_b <= upper_b
          label_b <- paste0(input$stat_input_b, " in [", lower_b, ", ", upper_b, "]")
        } else {
          ref_b <- input$reference_line_b
          cond_b <- !is.na(vals_b) & vals_b >= ref_b
          label_b <- paste0(input$stat_input_b, " >= ", ref_b)
        }
        p_b <- mean(cond_b, na.rm = TRUE)

        # Combined (AND)
        cond_combined <- cond_a & cond_b
        op_symbol <- " ∩ "
        label_combined <- paste0("(", label_a, ") AND (", label_b, ")")
        p_combined <- mean(cond_combined, na.rm = TRUE)

        # Joint under probability: both are under their respective lines
        if (identical(input$probability_mode_a, "Interval")) {
          under_a <- !is.na(vals_a) & vals_a < input$interval_lower_a
        } else {
          under_a <- !is.na(vals_a) & vals_a < input$reference_line
        }
        if (identical(input$probability_mode_b, "Interval")) {
          under_b <- !is.na(vals_b) & vals_b < input$interval_lower_b
        } else {
          under_b <- !is.na(vals_b) & vals_b < input$reference_line_b
        }
        p_both_under <- mean(under_a & under_b, na.rm = TRUE)

        parts <- c(
          parts,
          paste0("P(", label_b, ") = ", round(p_b, 2),
                 " | Over Odds: ", ifelse(is.finite(1/p_b), round(1/p_b, 2), "Inf"),
                 ", Under Odds: ", ifelse(is.finite(1/(1-p_b)), round(1/(1-p_b), 2), "Inf")),
          paste0("P(", label_combined, ") = ", round(p_combined, 2),
                 " | Over Odds: ", ifelse(is.finite(1/p_combined), round(1/p_combined, 2), "Inf"),
                 ", Under Odds (both): ", ifelse(is.finite(1/p_both_under), round(1/p_both_under, 2), "Inf"))
        )
      }
    }

    paste(parts, collapse = "\n")
  })
  
  #=============================================================================
  # Plot player stats
  #=============================================================================
  
  output$plots_container <- renderUI({
    if (isTRUE(input$enable_second_stat)) {
      tags$div(style = "display:flex; gap: 16px;",
               tags$div(style = "flex:1;", plotOutput("plot_a", height = "800px")),
               tags$div(style = "flex:1;", plotOutput("plot_b", height = "800px"))
      )
    } else {
      plotOutput("plot_a", height = "800px")
    }
  })

  output$plot_a <- renderPlot({
    df <- filtered_player_stats()
    if (nrow(df) == 0) return(NULL)

    vals <- df[[input$stat_input_a]]
    if (identical(input$probability_mode_a, "Interval")) {
      cond <- !is.na(vals) & vals >= input$interval_lower_a & vals <= input$interval_upper_a
    } else {
      cond <- !is.na(vals) & vals >= input$reference_line
    }

    df_with_color <- df %>% mutate(color_condition = ifelse(cond, "limegreen", "red1"))

    p <- df_with_color %>%
      ggplot(aes(x = game_number, y = !!sym(input$stat_input_a), color = color_condition)) +
      geom_point(size = 3) +
      geom_smooth(method = "loess", se = TRUE, inherit.aes = FALSE,
                  mapping = aes(x = game_number, y = !!sym(input$stat_input_a)))

    if (identical(input$probability_mode_a, "Interval")) {
      p <- p +
        geom_hline(yintercept = input$interval_lower_a, linetype = "dashed", color = "grey4", size = 0.8) +
        geom_hline(yintercept = input$interval_upper_a, linetype = "dashed", color = "grey4", size = 0.8)
    } else {
      p <- p + geom_hline(yintercept = input$reference_line, linetype = "dashed", color = "grey4", size = 1)
    }

    p <- p + annotate(
      geom = "text",
      x = 1,
      y = max(df %>% pull(!!sym(input$stat_input_a)), na.rm = TRUE),
      label = probability_summary(),
      hjust = 0, vjust = 1, color = "black", size = 5
    ) +
      theme_bw() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) +
      labs(title = "", x = "Game Number") +
      scale_color_identity() +
      theme(legend.position = "none")

    print(p)
  })

  output$plot_b <- renderPlot({
    req(input$enable_second_stat)
    df <- filtered_player_stats()
    if (nrow(df) == 0) return(NULL)

    vals <- df[[input$stat_input_b]]
    if (identical(input$probability_mode_b, "Interval")) {
      cond <- !is.na(vals) & vals >= input$interval_lower_b & vals <= input$interval_upper_b
      label_b <- paste0(input$stat_input_b, " in [", input$interval_lower_b, ", ", input$interval_upper_b, "]")
    } else {
      cond <- !is.na(vals) & vals >= input$reference_line_b
      label_b <- paste0(input$stat_input_b, " >= ", input$reference_line_b)
    }
    p_b <- mean(cond, na.rm = TRUE)

    df_with_color <- df %>% mutate(color_condition = ifelse(cond, "limegreen", "red1"))

    p <- df_with_color %>%
      ggplot(aes(x = game_number, y = !!sym(input$stat_input_b), color = color_condition)) +
      geom_point(size = 3) +
      geom_smooth(method = "loess", se = TRUE, inherit.aes = FALSE,
                  mapping = aes(x = game_number, y = !!sym(input$stat_input_b)))

    if (identical(input$probability_mode_b, "Interval")) {
      p <- p +
        geom_hline(yintercept = input$interval_lower_b, linetype = "dashed", color = "grey4", size = 0.8) +
        geom_hline(yintercept = input$interval_upper_b, linetype = "dashed", color = "grey4", size = 0.8)
    } else {
      p <- p + geom_hline(yintercept = input$reference_line_b, linetype = "dashed", color = "grey4", size = 1)
    }

    # Local annotation for B only
    annot <- paste0(
      "P(", label_b, ") = ", round(p_b, 2),
      " | Over Odds: ", ifelse(is.finite(1/p_b), round(1/p_b, 2), "Inf"),
      ", Under Odds: ", ifelse(is.finite(1/(1-p_b)), round(1/(1-p_b), 2), "Inf"),
      "\nSample Size: ", nrow(df)
    )

    p <- p + annotate(
      geom = "text",
      x = 1,
      y = max(df %>% pull(!!sym(input$stat_input_b)), na.rm = TRUE),
      label = annot,
      hjust = 0, vjust = 1, color = "black", size = 5
    ) +
      theme_bw() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) +
      labs(title = "", x = "Game Number") +
      scale_color_identity() +
      theme(legend.position = "none")

    print(p)
  })
  
  #=============================================================================
  # Table player stats
  #=============================================================================
  
  output$player_stat_table <- renderDT({
    datatable(
      filtered_player_stats(),
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
      width = "100%",
      height = "800px"
    )
  })
  
  #=============================================================================
  # Filter team stats
  #=============================================================================
  # 
  # # Reactive function to filter team stats
  # filtered_team_stats <- reactive({
  #   
  #   # Filter team stats
  #   filtered_team_stats <-
  #     all_team_stats |>
  #     filter(season %in% input$season_input_b)
  # 
  #     # Filter by last n games
  #     if (!is.na(input$last_games_team)) {
  #       filtered_team_stats <-
  #         filtered_team_stats |>
  #         group_by(teamId) |> 
  #         slice_head(n = input$last_games_team) |> 
  #         ungroup()
  #     }
  #     
  #     # Summarise stats
  #     filtered_team_stats <-
  #       filtered_team_stats |>
  #       select(
  #         teamName,
  #         possessions,
  #         pacePer40,
  #         offensiveRating,
  #         defensiveRating,
  #         netRating,
  #         assistPercentage,
  #         defensiveReboundPercentage,
  #         offensiveReboundPercentage,
  #         reboundPercentage,
  #         trueShootingPercentage,
  #         effectiveFieldGoalPercentage
  #       ) |> 
  #     group_by(teamName) |>
  #       summarise(across(.cols = where(is.numeric),
  #                        .fns = list(mean = mean))) |> 
  #       mutate(across(.cols = where(is.numeric), .fns = round, 2))
  #     
  #     # Return filtered team stats
  #     return(filtered_team_stats)
  #   
  # })
  
  #=============================================================================
  # Table team stats
  #=============================================================================
  # 
  # output$team_metric_table <- renderDT({
  #   datatable(
  #     filtered_team_stats(),
  #     options = list(pageLength = 15, autoWidth = TRUE),
  #     width = "100%",
  #     height = "800px"
  #   )
  # })
  # 
  #=============================================================================
  # Table Odds
  #=============================================================================
  
  # Reactive function to scrape odds
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------
    
    # Points
    if (input$market_input == "Points") {
      odds <-
        player_points_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }

    # Rebounds
    if (input$market_input == "Rebounds") {
      odds <-
        player_rebounds_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>

        select(-match) 
    }

    # Assists
    if (input$market_input == "Assists") {
      odds <-
        player_assists_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }
      
    # Threes
    if (input$market_input == "Threes") {
      odds <-
        player_threes_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }

    # PRAs
    if (input$market_input == "PRAs") {
      odds <-
        player_pras_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }

    # Steals
    if (input$market_input == "Steals") {
      odds <-
        player_steals_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }

    # Blocks
    if (input$market_input == "Blocks") {
      odds <-
        player_blocks_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    if (input$only_best == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(over_price)) |>
        group_by(player_name, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    # Min and max differences
    if (!is.na(input$diff_minimum_23)) {
      odds <-
        odds |>
        filter(diff_over_2023_24 >= input$diff_minimum_23)
    }
    
    if (!is.na(input$diff_maximum_23)) {
      odds <-
        odds |>
        filter(diff_over_2023_24 <= input$diff_maximum_23)
    }
    
    if (!is.na(input$diff_minimum_24)) {
      odds <-
        odds |>
        filter(diff_over_2024_25 >= input$diff_minimum_24)
    }
    
    if (!is.na(input$diff_maximum_24)) {
      odds <-
        odds |>
        filter(diff_over_2024_25 <= input$diff_maximum_24)
    }
    
    # Odds Range
        if (!is.na(input$odds_minimum)) {
      odds <-
        odds |>
        filter(over_price >= input$odds_minimum)
    }
    
    if (!is.na(input$odds_maximum)) {
      odds <-
        odds |>
        filter(over_price <= input$odds_maximum)
    }
    
    if (input$only_unders == TRUE) {
      odds <-
        odds |>
        filter(!is.na(under_price))
    }
    
    if (input$player_name_input_b != "") {
      odds <-
        odds |>
        filter(str_detect(player_name, input$player_name_input_b))
    }
      
    odds <-
        odds |> 
        select(-contains("key")) |>
        relocate(agency, .before = player_name)
      
    # Return odds
    return(odds)
  })

  #=============================================================================
  # DVP Heatmap
  #=============================================================================

  dvp_active_df <- reactive({
    req(input$dvp_stat)
    df <- switch(
      input$dvp_stat,
      "Points"   = dvp_points,
      "Rebounds" = dvp_rebounds,
      "Assists"  = dvp_assists,
      "Threes"   = dvp_threes,
      "Steals"   = dvp_steals,
      "Blocks"   = dvp_blocks,
      "PRAs"     = dvp_pras
    )
    if (!is.null(input$dvp_min_games) && !is.na(input$dvp_min_games)) {
      df <- df |> filter(games >= input$dvp_min_games)
    }
    df
  })

  output$dvp_heatmap <- renderPlot({
    df <- dvp_active_df()
    if (nrow(df) == 0) return(NULL)

    # Determine fill column
    fill_col <- case_when(
      identical(input$dvp_stat, "Points")   ~ "avg_points",
      identical(input$dvp_stat, "Rebounds") ~ "avg_rebounds",
      identical(input$dvp_stat, "Assists")  ~ "avg_assists",
      identical(input$dvp_stat, "Threes")   ~ "avg_threes",
      identical(input$dvp_stat, "Steals")   ~ "avg_steals",
      identical(input$dvp_stat, "Blocks")   ~ "avg_blocks",
      TRUE                                   ~ "avg_pras"
    )

    p <- ggplot(df, aes(x = position, y = opposition, fill = .data[[fill_col]])) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete(position = "top") +
      labs(x = NULL, y = NULL, title = paste("DVP:", input$dvp_stat), fill = "Avg Diff")

    if (isTRUE(input$dvp_show_labels)) {
      p <- p + geom_text(aes(label = round(.data[[fill_col]], 1)), size = 3)
    }

    p
  })
  
  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
              options = list(
                pageLength = 12,
                autoWidth = FALSE,
                scrollX = TRUE, scrollY = TRUE,
                lengthMenu = c(5, 10, 12, 15, 20, 25, 30)
              ))
  })
  
  #=============================================================================
  # With / Without Teammate
  #=============================================================================
  
  output$with_without_plot_output <- renderPlot({
    req(input$player_name, input$teammate_name, input$season_input, input$metric_input)
    
    plot <- compare_performance(season = input$season_input,
                                main_name = input$player_name,
                                teammate_name = input$teammate_name,
                                metric = input$metric_input)
    
    return(plot)
  })
  
  #=============================================================================
  # Player Correlations
  #=============================================================================
  
  output$corr_plot_output <- renderPlot({
    req(input$player_name_corr, input$teammate_name_corr, input$season_input_corr, input$metric_input_corr_b, input$metric_input_corr_a)
    
    plot <-
      get_player_correlation(
        seasons = input$season_input_corr,
        name_a = input$player_name_corr,
        name_b = input$teammate_name_corr,
        metric_a = input$metric_input_corr_a,
        metric_b = input$metric_input_corr_b
      )
    
    return(plot)
  })
  
}

#===============================================================================
# Run App
#===============================================================================

shinyApp(ui, server)

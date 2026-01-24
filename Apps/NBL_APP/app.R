library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(readr)
library(lubridate)
library(plotly)
library(httr)
library(jsonlite)

# Helper functions
`%notin%` <- Negate(`%in%`)

# Helper: normalize team names for safer joins
normalize_team <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]", "", x)
  x
}

# ===============================================================================
# Read in Data
# ===============================================================================

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
    SEASON_YEAR = season
  ) |>
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
  dvp_points <- dvp_results$points
  dvp_rebounds <- dvp_results$rebounds
  dvp_assists <- dvp_results$assists
  dvp_threes <- dvp_results$threes
  dvp_steals <- dvp_results$steals
  dvp_blocks <- dvp_results$blocks
  dvp_pras <- dvp_results$pras
} else {
  dvp_points <- tibble(position = character(), opposition = character(), games = integer(), avg_points = numeric())
  dvp_rebounds <- tibble(position = character(), opposition = character(), games = integer(), avg_rebounds = numeric())
  dvp_assists <- tibble(position = character(), opposition = character(), games = integer(), avg_assists = numeric())
  dvp_threes <- tibble(position = character(), opposition = character(), games = integer(), avg_threes = numeric())
  dvp_steals <- tibble(position = character(), opposition = character(), games = integer(), avg_steals = numeric())
  dvp_blocks <- tibble(position = character(), opposition = character(), games = integer(), avg_blocks = numeric())
  dvp_pras <- tibble(position = character(), opposition = character(), games = integer(), avg_pras = numeric())
}

# Read Odds Data----------------------------------------------------------------
player_points_data <- read_rds("../../Data/processed_odds/all_player_points.rds")
player_assists_data <- read_rds("../../Data/processed_odds/all_player_assists.rds")
player_rebounds_data <- read_rds("../../Data/processed_odds/all_player_rebounds.rds")
player_threes_data <- read_rds("../../Data/processed_odds/all_player_threes.rds")

# New markets
player_pras_data <- tryCatch(read_rds("../../Data/processed_odds/all_player_pras.rds"), error = function(e) tibble())
player_steals_data <- tryCatch(read_rds("../../Data/processed_odds/all_player_steals.rds"), error = function(e) tibble())
player_blocks_data <- tryCatch(read_rds("../../Data/processed_odds/all_player_blocks.rds"), error = function(e) tibble())

# Aggregate choices
all_agencies <- unique(c(
  player_points_data$agency, player_assists_data$agency, player_rebounds_data$agency, player_threes_data$agency,
  player_pras_data$agency, player_steals_data$agency, player_blocks_data$agency
))
all_agencies <- all_agencies[!is.na(all_agencies)]
all_matches <- unique(c(
  player_points_data$match, player_assists_data$match, player_rebounds_data$match, player_threes_data$match,
  player_pras_data$match, player_steals_data$match, player_blocks_data$match
))
all_matches <- all_matches[!is.na(all_matches)]

# ===============================================================================
# SGM Data Loading
# ===============================================================================

# Source SGM helper files
source("R/betright_sgm.R")
source("R/tab_sgm.R")
source("R/sportsbet_sgm.R")
source("R/pointsbet_sgm.R")
source("R/bet365_sgm.R")
source("R/dabble_sgm.R")

# Head to head data (used for match ordering)
h2h <- tryCatch(read_csv("../../Data/scraped_odds/tab_h2h.csv"), error = function(e) tibble(match = character()))

# Matches in order
matches_in_order <-
  h2h %>%
  distinct(match) |>
  pull()

# ===============================================================================
# SGM Compare Function
# ===============================================================================

compare_sgm <- function(player_names, stat_counts, markets, types) {
  # Function to handle errors in the call_sgm functions
  handle_call_sgm <- function(func, sgm, player_names, stat_counts, markets, types) {
    tryCatch({
      func(sgm, player_names, stat_counts, markets, types)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections=NA, Markets = NA, Unadjusted_Price=NA, Adjusted_Price=NA, Adjustment_Factor=NA, Agency=NA)
    })
  }

  # Get individual dataframes
  pointsbet_data <- handle_call_sgm(call_sgm_pointsbet, pointsbet_sgm, player_names, stat_counts, markets, types)
  sportsbet_data <- handle_call_sgm(call_sgm_sportsbet, sportsbet_sgm, player_names, stat_counts, markets, types)
  tab_data <- handle_call_sgm(call_sgm_tab, tab_sgm, player_names, stat_counts, markets, types)
  betright_data <- handle_call_sgm(call_sgm_betright, betright_sgm, player_names, stat_counts, markets, types)
  bet365_data <- handle_call_sgm(call_sgm_bet365, bet365_sgm, player_names, stat_counts, markets, types)
  dabble_data <- handle_call_sgm(call_sgm_dabble, dabble_sgm, player_names, stat_counts, markets, types)

  # Bind together and return
  bind_rows(pointsbet_data, sportsbet_data, tab_data, betright_data, bet365_data, dabble_data) |>
    mutate(Adjusted_Price = round(Adjusted_Price, 2),
           Unadjusted_Price = round(Unadjusted_Price, 2),
           Adjustment_Factor = round(Adjustment_Factor, 2)
           ) |>
    arrange(desc(Adjusted_Price))
}

# ===============================================================================
# Compare CGM function
# ===============================================================================

compare_cgm <- function(player_names_cross, lines_cross, market_names_cross, types_cross) {
  # List of each agency data
  all_data <- list(pointsbet_sgm, sportsbet_sgm, tab_sgm, betright_sgm, bet365_sgm, dabble_sgm)

  # Function to get cross game multi data
  get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross, types_cross) {
    if (length(player_names_cross) != length(lines_cross) || length(lines_cross) != length(market_names_cross)) {
      stop("All lists should have the same length")
    }

    filtered_df <- data.frame()
    for (i in seq_along(player_names_cross)) {
      temp_df <- data %>%
        filter(player_name == player_names_cross[i],
               line == lines_cross[i],
               market_name == market_names_cross[i],
               type == types_cross[i])
      filtered_df <- bind_rows(filtered_df, temp_df)
    }

    if (nrow(filtered_df) != length(player_names_cross)) {
      return(NULL)
    }

    price <- prod(filtered_df$price)

    combined_list <- paste(player_names_cross, lines_cross, sep = ": ")
    player_string <- paste(combined_list, collapse = ", ")
    market_string <- paste(market_names_cross, collapse = ", ")
    match_string <- paste(filtered_df$match, collapse = ", ")

    output_data <- data.frame(
      Selections = player_string,
      Matches = match_string,
      Markets = market_string,
      Price = round(price, 2),
      Agency = first(data$agency)
    )

    return(output_data)
  }

  # Function to handle errors in the get_cgm function
  handle_get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross, types_cross) {
    tryCatch({
      get_cgm(data, player_names_cross, lines_cross, market_names_cross, types_cross)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections = NA, Matches = NA, Markets = NA, Price = NA, Agency = NA)
    })
  }

  # Map over list of dataframes
  cgm_all <- map_dfr(all_data, handle_get_cgm, player_names_cross, lines_cross, market_names_cross, types_cross) %>%
    arrange(desc(Price))

  return(cgm_all)
}

# ===============================================================================
# SGM Data Preparation
# ===============================================================================

# Load DVP bundle and positions (if available)
positions_sc <- tryCatch(read_csv("../../Data/supercoach-data.csv") |>
                           select(player_name, player_team, position = supercoach_position_1) |>
                           filter(!is.na(position)), error = function(e) NULL)

if (!is.null(dvp_results)) {
  dvp_points_sgm   <- dvp_results$points |> rename(dvp_value = avg_points, dvp_games = games)
  dvp_rebounds_sgm <- dvp_results$rebounds |> rename(dvp_value = avg_rebounds, dvp_games = games)
  dvp_assists_sgm  <- dvp_results$assists |> rename(dvp_value = avg_assists, dvp_games = games)
  dvp_threes_sgm   <- dvp_results$threes |> rename(dvp_value = avg_threes, dvp_games = games)
  dvp_steals_sgm   <- dvp_results$steals |> rename(dvp_value = avg_steals, dvp_games = games)
  dvp_blocks_sgm   <- dvp_results$blocks |> rename(dvp_value = avg_blocks, dvp_games = games)
  dvp_pras_sgm     <- dvp_results$pras   |> rename(dvp_value = avg_pras,   dvp_games = games)

  dvp_long <- bind_rows(
    dvp_points_sgm |> mutate(stat_key = "Player Points"),
    dvp_rebounds_sgm |> mutate(stat_key = "Player Rebounds"),
    dvp_assists_sgm |> mutate(stat_key = "Player Assists"),
    dvp_threes_sgm |> mutate(stat_key = "Player Threes"),
    dvp_steals_sgm |> mutate(stat_key = "Player Steals"),
    dvp_blocks_sgm |> mutate(stat_key = "Player Blocks"),
    dvp_pras_sgm   |> mutate(stat_key = "Player PRAs")
  ) |> mutate(oppo_norm = normalize_team(opposition))
} else {
  dvp_long <- tibble(position = character(), opposition = character(), dvp_value = numeric(), dvp_games = integer(), stat_key = character(), oppo_norm = character())
}

# Combine NBL props then expand to Over/Under rows with unified fields
props_all <- bind_rows(player_points_data, player_rebounds_data, player_assists_data, player_threes_data, player_pras_data, player_steals_data, player_blocks_data)

overs <- props_all |>
  mutate(type = "Over",
         price = over_price,
         prob_s2025 = empirical_prob_over_2025_26,
         diff_2025 = diff_over_2025_26,
         prob_last_10 = empirical_prob_over_last_10,
         diff_last_10 = diff_over_last_10)

unders <- props_all |>
  filter(!is.na(under_price)) |>
  mutate(type = "Under",
         price = under_price,
         prob_2025 = empirical_prob_under_2025_26,
         diff_2025 = diff_under_2025_26,
         prob_last_10 = empirical_prob_under_last_10,
         diff_last_10 = diff_under_last_10)

disposals <- bind_rows(overs, unders)

# Add position and DVP join if available
if (!is.null(positions_sc) && nrow(dvp_long) > 0) {
  # Primary: join on name + team
  disposals <-
    disposals |>
    left_join(positions_sc, by = c("player_name", "player_team"))

  # Fallback: if position missing, join on name only
  missing_pos <- is.na(disposals$position)
  if (any(missing_pos)) {
    pos_by_name <- positions_sc |> select(player_name, position) |> distinct()
    disposals <- disposals |>
      left_join(pos_by_name, by = c("player_name"), suffix = c("", "_by_name")) |>
      mutate(position = coalesce(position, position_by_name)) |>
      select(-position_by_name)
  }

  # Map stat key from market_name then join DVP
  disposals <-
    disposals |>
    mutate(stat_key = market_name,
           oppo_norm = normalize_team(opposition_team)) |>
    left_join(dvp_long, by = c("position", "oppo_norm", "stat_key"))
} else {
  disposals <- disposals |> mutate(dvp_value = NA_real_, dvp_games = NA_integer_)
}

# Create market best
disposals <-
  disposals |>
  group_by(match, player_name, market_name, line, type) |>
  arrange(desc(price), .by_group = TRUE) |>
  mutate(
    max_player_diff = max(diff_last_10, na.rm = TRUE),
    second_best_price = if_else(n() >= 2, nth(price, 2), NA_real_),
    market_best = row_number() == 1
  ) |>
  ungroup()

# Unique matches (prefer H2H order; fallback to props if H2H empty)
matches_from_props <-
  disposals |>
  filter(!is.na(match)) |>
  distinct(match) |>
  pull() |>
  unique()

if (length(matches_in_order) == 0) {
  sgm_matches <- matches_from_props
} else {
  # Use H2H order but include any matches present only in props
  extra_matches <- setdiff(matches_from_props, matches_in_order)
  sgm_matches <- c(matches_in_order, extra_matches)
}

# Unique agencies for SGM
sgm_agencies <-
  disposals |>
  distinct(agency) |>
  pull() |>
  unique()

# Create disposals dataframe to display
disposals_display <-
  disposals |>
  group_by(player_name, match, line, market_name) |>
  mutate(
    next_best_diff = if_else(market_best,
                            ((1/second_best_price) - (1/price)),
                             NA_real_)
  ) |>
  ungroup() |>
  arrange(desc(max_player_diff)) |>
  transmute(match,
         player_name,
         position,
         type,
         market_name,
         line,
         price,
         agency,
         dvp = round(dvp_value, 2),
         dvp_games,
         prob_2025 = round(prob_2025, 2),
         diff_2025 = round(diff_2025, 2),
         prob_last_10 = round(prob_last_10, 2),
         diff_last_10 = round(diff_last_10, 2),
         next_best_diff = round(100 * next_best_diff, 1),
         market_best)

# ===============================================================================
# UI
# ===============================================================================

ui <- page_navbar(
  title = "NBL",
  selected = "Player Stats",
  collapsible = TRUE,
  theme = bslib::bs_theme(preset = "zephyr"),
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
            choices = c(
              "PTS",
              "REB",
              "AST",
              "PRA",
              "FG3M",
              "BLK",
              "STL",
              "MIN"
            ),
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
      grid_card(
        area = "player_stat_plot",
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
        )
      )
    )
  ),
  nav_panel(
    title = "Odds Screen",
    grid_container(
      layout = c("odds_screen odds_table"),
      row_sizes = c("1fr"),
      col_sizes = c(
        "250px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "odds_screen",
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
            multiple = TRUE,
            selected = "Points"
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
        )
      ),
      grid_card(
        area = "odds_table",
        card_body(
          DTOutput(outputId = "scraped_odds_table", height = "1500px")
        )
      )
    )
  ),
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
          plotlyOutput("dvp_heatmap", height = "900px")
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
            choices = all_player_stats$PLAYER_NAME |> unique() |> sort(),
            selectize = TRUE
          ),
          selectInput(
            inputId = "teammate_name",
            label = "Select Teammate:",
            selected = "Keanu Pinder",
            choices = all_player_stats$PLAYER_NAME |> unique() |> sort(),
            selectize = TRUE
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
      grid_card(
        area = "with_without_plot",
        card_body(
          tabsetPanel(
            id = "with_without_tabs",
            tabPanel(
              "Plot",
              plotlyOutput(outputId = "with_without_plot_output", height = "800px", width = "100%")
            ),
            tabPanel(
              "Table",
              DTOutput(outputId = "with_without_table_output", width = "100%", height = "800px")
            )
          )
        )
      )
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
      grid_card(
        area = "corr_plot",
        card_body(
          plotlyOutput(outputId = "corr_plot_output", height = "800px", width = "100%")
        )
      )
    )
  ),
  # =============================================================================
  # SGM Tab
  # =============================================================================
  nav_panel(
    title = "SGM",
    fluidRow(
      column(3,
        wellPanel(
          selectInput(
            "sgm_match",
            "Select Match",
            choices = sgm_matches,
            selected = if(length(sgm_matches) > 0) sgm_matches[1] else NULL
          ),
          selectInput(
            "sgm_agency",
            "Select Agency",
            choices = sgm_agencies,
            selected = if(length(sgm_agencies) > 0) sgm_agencies[1] else NULL
          ),
          selectInput(
            "sgm_market",
            "Select Market",
             choices = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes", "Player PRAs", "Player Steals", "Player Blocks"),
             selected = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes", "Player PRAs", "Player Steals", "Player Blocks"),
             multiple = TRUE
          ),
          checkboxInput("sgm_best_odds", "Only Show Best Market Odds?", value = FALSE),
          hr(),
          h5("Matchup Filters (DVP)"),
          radioButtons(
            "sgm_dvp_type",
            "Matchup Type",
            choices = c("All", "Good (>= 0)", "Bad (<= 0)"),
            selected = "All"
          ),
          sliderInput(
            "sgm_dvp_range",
            "DVP Range",
            min = -5, max = 5, value = c(-5, 5), step = 0.1
          ),
          numericInput(
            "sgm_dvp_min_games",
            "Min DVP sample size",
            value = 0, min = 0, step = 1
          ),
          checkboxInput(
            "sgm_dvp_good_by_type",
            "Good matchup by bet type (Over=+, Under=-)",
            value = FALSE
          ),
          hr(),
          h4("Selections"),
          DT::dataTableOutput("sgm_selected"),
          h4("SGM Information"),
          uiOutput("sgm_summary"),
          h4("Odds Comparison"),
          actionButton("sgm_get_comparison", label = "Compare Odds"),
          actionButton("sgm_clear_comparison", label = "Clear Selections"),
          DT::dataTableOutput("sgm_odds_compare")
        )
      ),
      column(9,
        DT::dataTableOutput("sgm_table")
      )
    )
  ),
  # =============================================================================
  # Cross Game Multi Tab
  # =============================================================================
  nav_panel(
    title = "Cross Game Multi",
    fluidRow(
      column(3,
        wellPanel(
          selectInput(
            "cgm_agency",
            "Select Agency",
            choices = sgm_agencies,
            selected = if(length(sgm_agencies) > 0) sgm_agencies[1] else NULL
          ),
          selectInput(
            "cgm_market",
            "Select Market",
             choices = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes", "Player PRAs", "Player Steals", "Player Blocks"),
             selected = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes", "Player PRAs", "Player Steals", "Player Blocks"),
             multiple = TRUE
           ),
          checkboxInput("cgm_best_odds", "Only Show Best Market Odds?", value = FALSE),
          hr(),
          h4("Selections"),
          DT::dataTableOutput("cgm_selected"),
          h4("Multi Information"),
          uiOutput("cgm_summary"),
          actionButton("cgm_get_comparison", label = "Compare Odds"),
          actionButton("cgm_clear_comparison", label = "Clear Selections"),
          DT::dataTableOutput("cgm_odds_compare")
        )
      ),
      column(9,
        DT::dataTableOutput("cgm_table")
      )
    )
  )
)


# ===============================================================================
# Server
# ===============================================================================

server <- function(input, output, session) {
  # =============================================================================
  # Filter player stats
  # =============================================================================

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
      select(
        Date = match_time_utc,
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
        game_number
      ) |>
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

  # =============================================================================
  # Get Proportion above reference line
  # =============================================================================

  probability_summary <- reactive({
    df <- filtered_player_stats()
    n <- nrow(df)
    if (n == 0) {
      return("No games after filters")
    }

    # Condition A
    vals_a <- df[[input$stat_input_a]]
    if (is.null(vals_a)) {
      return("Invalid primary stat selection")
    }

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
      paste0(
        "P(", label_a, ") = ", round(p_a, 2),
        " | Over Odds: ", ifelse(is.finite(1 / p_a), round(1 / p_a, 2), "Inf"),
        ", Under Odds: ", ifelse(is.finite(1 / (1 - p_a)), round(1 / (1 - p_a), 2), "Inf")
      ),
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
          paste0(
            "P(", label_b, ") = ", round(p_b, 2),
            " | Over Odds: ", ifelse(is.finite(1 / p_b), round(1 / p_b, 2), "Inf"),
            ", Under Odds: ", ifelse(is.finite(1 / (1 - p_b)), round(1 / (1 - p_b), 2), "Inf")
          ),
          paste0(
            "P(", label_combined, ") = ", round(p_combined, 2),
            " | Over Odds: ", ifelse(is.finite(1 / p_combined), round(1 / p_combined, 2), "Inf"),
            ", Under Odds (both): ", ifelse(is.finite(1 / p_both_under), round(1 / p_both_under, 2), "Inf")
          )
        )
      }
    }

    paste(parts, collapse = "\n")
  })

  # =============================================================================
  # Plot player stats
  # =============================================================================

  output$plots_container <- renderUI({
    if (isTRUE(input$enable_second_stat)) {
      tagList(
        card(
          card_header("Probability Summary"),
          verbatimTextOutput("stats_summary_text")
        ),
        tags$div(
          style = "display:flex; gap: 16px;",
          tags$div(style = "flex:1;", plotlyOutput("plot_a", height = "800px")),
          tags$div(style = "flex:1;", plotlyOutput("plot_b", height = "800px"))
        )
      )
    } else {
      tagList(
        card(
          card_header("Probability Summary"),
          verbatimTextOutput("stats_summary_text")
        ),
        plotlyOutput("plot_a", height = "800px")
      )
    }
  })

  output$stats_summary_text <- renderText({
    probability_summary()
  })

  output$plot_a <- renderPlotly({
    df <- filtered_player_stats()
    if (nrow(df) == 0) {
      return(NULL)
    }

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
      geom_smooth(
        method = "loess", se = TRUE, inherit.aes = FALSE,
        mapping = aes(x = game_number, y = !!sym(input$stat_input_a))
      )

    if (identical(input$probability_mode_a, "Interval")) {
      p <- p +
        geom_hline(yintercept = input$interval_lower_a, linetype = "dashed", color = "grey4", size = 0.8) +
        geom_hline(yintercept = input$interval_upper_a, linetype = "dashed", color = "grey4", size = 0.8)
    } else {
      p <- p + geom_hline(yintercept = input$reference_line, linetype = "dashed", color = "grey4", size = 1)
    }

    p <- p +
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      labs(title = "", x = "Game Number") +
      scale_color_identity() +
      theme(legend.position = "none")

    ggplotly(p)
  })

  output$plot_b <- renderPlotly({
    req(input$enable_second_stat)
    df <- filtered_player_stats()
    if (nrow(df) == 0) {
      return(NULL)
    }

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
      geom_smooth(
        method = "loess", se = TRUE, inherit.aes = FALSE,
        mapping = aes(x = game_number, y = !!sym(input$stat_input_b))
      )

    if (identical(input$probability_mode_b, "Interval")) {
      p <- p +
        geom_hline(yintercept = input$interval_lower_b, linetype = "dashed", color = "grey4", size = 0.8) +
        geom_hline(yintercept = input$interval_upper_b, linetype = "dashed", color = "grey4", size = 0.8)
    } else {
      p <- p + geom_hline(yintercept = input$reference_line_b, linetype = "dashed", color = "grey4", size = 1)
    }

    p <- p +
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      labs(title = "", x = "Game Number") +
      scale_color_identity() +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # =============================================================================
  # Table player stats
  # =============================================================================

  output$player_stat_table <- renderDT({
    datatable(
      filtered_player_stats(),
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
      width = "100%",
      height = "800px"
    )
  })

  # =============================================================================
  # Table Odds
  # =============================================================================

  # Reactive function to scrape odds
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------

    # Combine data based on selection
    data_map <- list(
      "Points" = player_points_data,
      "Rebounds" = player_rebounds_data,
      "Assists" = player_assists_data,
      "Threes" = player_threes_data,
      "PRAs" = player_pras_data,
      "Steals" = player_steals_data,
      "Blocks" = player_blocks_data
    )

    # Filter for selected markets and bind rows
    selected_data <- data_map[input$market_input]
    odds <- bind_rows(selected_data)

    # Apply common transformations
    odds <-
      odds |>
      mutate(variation = round(variation, 2)) |>
      filter(agency %in% input$agency_input) |>
      filter(match %in% input$match_input) |>
      select(-match)

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

  # =============================================================================
  # DVP Heatmap
  # =============================================================================

  dvp_active_df <- reactive({
    req(input$dvp_stat)
    df <- switch(input$dvp_stat,
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

  output$dvp_heatmap <- renderPlotly({
    df <- dvp_active_df()
    if (nrow(df) == 0) {
      return(NULL)
    }

    # Determine fill column
    fill_col <- case_when(
      identical(input$dvp_stat, "Points") ~ "avg_points",
      identical(input$dvp_stat, "Rebounds") ~ "avg_rebounds",
      identical(input$dvp_stat, "Assists") ~ "avg_assists",
      identical(input$dvp_stat, "Threes") ~ "avg_threes",
      identical(input$dvp_stat, "Steals") ~ "avg_steals",
      identical(input$dvp_stat, "Blocks") ~ "avg_blocks",
      TRUE ~ "avg_pras"
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

    ggplotly(p)
  })

  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
      options = list(
        pageLength = 12,
        autoWidth = FALSE,
        scrollX = TRUE, scrollY = TRUE,
        lengthMenu = c(5, 10, 12, 15, 20, 25, 30)
      )
    )
  })

  # =============================================================================
  # With / Without Teammate
  # =============================================================================

  # Reactive to get filtered player data with/without teammate
  with_without_data <- reactive({
    req(input$player_name, input$teammate_name, input$season_input)

    # Filter the data for games with the main player
    df_player <-
      all_player_stats %>%
      filter(PLAYER_NAME == input$player_name) %>%
      filter(SEASON_YEAR %in% input$season_input)

    # Find the game IDs where the teammate also played
    games_with_teammate <-
      all_player_stats %>%
      filter(SEASON_YEAR %in% input$season_input) %>%
      filter(PLAYER_NAME == input$teammate_name) %>%
      pull(match_id)

    # Label each game as 'With Teammate' or 'Without Teammate'
    df_player <- df_player %>%
      mutate(Teammate = if_else(match_id %in% games_with_teammate, 'With Teammate', 'Without Teammate'))

    return(df_player)
  })

  output$with_without_plot_output <- renderPlotly({
    req(input$player_name, input$teammate_name, input$season_input, input$metric_input)

    plot <- compare_performance(
      player_data = all_player_stats,
      season = input$season_input,
      main_name = input$player_name,
      teammate_name = input$teammate_name,
      metric = input$metric_input
    )

    return(ggplotly(plot))
  })

  output$with_without_table_output <- renderDT({
    req(input$player_name, input$teammate_name, input$season_input)

    df_player <- with_without_data()

    # Calculate summary stats for all major stats
    summary_table <- df_player %>%
      group_by(Teammate) %>%
      summarise(
        Games = n(),
        MIN = round(mean(MIN, na.rm = TRUE), 1),
        PTS = round(mean(PTS, na.rm = TRUE), 1),
        REB = round(mean(REB, na.rm = TRUE), 1),
        AST = round(mean(AST, na.rm = TRUE), 1),
        PRA = round(mean(PRA, na.rm = TRUE), 1),
        STL = round(mean(STL, na.rm = TRUE), 1),
        BLK = round(mean(BLK, na.rm = TRUE), 1),
        FGM = round(mean(player_field_goals_made, na.rm = TRUE), 1),
        FGA = round(mean(player_field_goals_attempted, na.rm = TRUE), 1),
        `FG%` = round(mean(player_field_goals_percentage, na.rm = TRUE), 1),
        FG3M = round(mean(player_three_pointers_made, na.rm = TRUE), 1),
        FG3A = round(mean(player_three_pointers_attempted, na.rm = TRUE), 1),
        `3P%` = round(mean(player_three_pointers_percentage, na.rm = TRUE), 1),
        FTM = round(mean(player_free_throws_made, na.rm = TRUE), 1),
        FTA = round(mean(player_free_throws_attempted, na.rm = TRUE), 1),
        `FT%` = round(mean(player_free_throws_percentage, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(Teammate))  # With Teammate first

    datatable(
      summary_table,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 't'  # Only show table, no search/pagination
      ),
      rownames = FALSE,
      width = "100%"
    )
  })

  # =============================================================================
  # Player Correlations
  # =============================================================================

  output$corr_plot_output <- renderPlotly({
    req(input$player_name_corr, input$teammate_name_corr, input$season_input_corr, input$metric_input_corr_b, input$metric_input_corr_a)

    plot <-
      get_player_correlation(
        player_data = all_player_stats,
        seasons = input$season_input_corr,
        name_a = input$player_name_corr,
        name_b = input$teammate_name_corr,
        metric_a = input$metric_input_corr_a,
        metric_b = input$metric_input_corr_b
      )

    return(ggplotly(plot))
  })

  # =============================================================================
  # SGM Tab Server Logic
  # =============================================================================

  # Helper to apply DVP-related filters consistently wherever we subset
  apply_dvp_filters <- function(df) {
    # Range filter
    if (!is.null(input$sgm_dvp_range) && length(input$sgm_dvp_range) == 2) {
      df <- df |>
        dplyr::filter(is.na(dvp) | (dvp >= input$sgm_dvp_range[1] & dvp <= input$sgm_dvp_range[2]))
    }
    # Min games filter
    if (!is.null(input$sgm_dvp_min_games) && input$sgm_dvp_min_games > 0) {
      df <- df |>
        dplyr::filter(!is.na(dvp_games) & dvp_games >= input$sgm_dvp_min_games)
    }
    # Type filter
    if (!is.null(input$sgm_dvp_type) && input$sgm_dvp_type != "All") {
      if (input$sgm_dvp_type == "Good (>= 0)") {
        df <- df |>
          dplyr::filter(!is.na(dvp) & dvp >= 0)
      } else if (input$sgm_dvp_type == "Bad (<= 0)") {
        df <- df |>
          dplyr::filter(!is.na(dvp) & dvp <= 0)
      }
    }
    # Good matchup by bet type: keep Over with positive DVP and Under with negative DVP
    if (isTRUE(input$sgm_dvp_good_by_type)) {
      df <- df |>
        dplyr::filter(!is.na(dvp) & ((type == "Over" & dvp >= 0) | (type == "Under" & dvp <= 0)))
    }
    df
  }

  # For the "SGM" panel
  output$sgm_table <- renderDT({
    filtered_data <-
      disposals_display[disposals_display$match == input$sgm_match &
                          disposals_display$agency == input$sgm_agency &
                          disposals_display$market_name %in% input$sgm_market,]

    # Apply DVP filters
    filtered_data <- apply_dvp_filters(filtered_data)

    if (input$sgm_best_odds) {
      filtered_data <- filtered_data |>
        filter(market_best) |>
        select(-market_best)
    } else {
      filtered_data <- filtered_data |> select(-next_best_diff)
    }

    datatable(filtered_data, selection = "multiple", filter = "top")
  }, server = FALSE) # We are setting this as FALSE for client-side processing of the DataTable

  # Update DVP slider range dynamically based on available data
  observe({
    dvp_vals <- suppressWarnings(range(disposals_display$dvp, na.rm = TRUE))
    if (all(is.finite(dvp_vals)) && diff(dvp_vals) >= 0) {
      updateSliderInput(session, "sgm_dvp_range",
                        min = floor(dvp_vals[1]),
                        max = ceiling(dvp_vals[2]),
                        value = c(floor(dvp_vals[1]), ceiling(dvp_vals[2])))
    }
  })


  observeEvent(input$sgm_table_rows_selected,{
    output$sgm_selected <- renderDT({
      if(!is.null(input$sgm_table_rows_selected)){
        filtered_data <-
          disposals_display[disposals_display$match == input$sgm_match &
                              disposals_display$agency == input$sgm_agency &
                              disposals_display$market_name %in% input$sgm_market,]
        # Apply DVP filters to keep row indices aligned with the main table
        filtered_data <- apply_dvp_filters(filtered_data)
        if (input$sgm_best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
        selected_data <- filtered_data[input$sgm_table_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data)
      }
    })
  })

  # Get the table proxy
  sgm_proxy <- dataTableProxy("sgm_table")

  # Get the table proxy for the cross game multi
  cgm_proxy <- dataTableProxy("cgm_table")

  # SGM Comparison
  observeEvent(input$sgm_get_comparison, {
    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$sgm_match &
                                         disposals_display$agency == input$sgm_agency &
                                         disposals_display$market_name %in% input$sgm_market,]
    # Apply DVP filters so SGM comparison uses the same subset
    filtered_data <- apply_dvp_filters(filtered_data)
    if (input$sgm_best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$sgm_table_rows_selected, c("player_name", "type", "line", "market_name", "price")]

    player_names = selected_data$player_name
    types = selected_data$type
    lines = selected_data$line
    market_names = selected_data$market_name

    # Call function
    comparison_df <- compare_sgm(
      player_names = player_names,
      stat_counts = lines,
      markets = market_names,
      types = types)

    # populate DTOutput
    output$sgm_odds_compare <- renderDT({
      datatable(comparison_df)
    })
  })

  # Observe the click event on the "clear_rows" button
  observeEvent(input$sgm_clear_comparison, {
    # Deselect all rows in the table
    selectRows(sgm_proxy, NULL)
  })

  observeEvent(input$cgm_clear_comparison, {
    # Deselect all rows in the table
    selectRows(cgm_proxy, NULL)
  })

  output$sgm_summary <- renderUI({
    if(!is.null(input$sgm_table_rows_selected)){
      filtered_data <- disposals_display[disposals_display$match == input$sgm_match &
                                           disposals_display$agency == input$sgm_agency &
                                           disposals_display$market_name %in% input$sgm_market,]
      # Apply DVP filters to align summary with selection table
      filtered_data <- apply_dvp_filters(filtered_data)
      if (input$sgm_best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
      selected_data <- filtered_data[input$sgm_table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_last_10)
      HTML(paste0("<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
                  " <strong>Theoretical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)))
    }
  })

  # =============================================================================
  # Cross Game Multi Tab Server Logic
  # =============================================================================

  # For the "Cross Game Multi" panel
  output$cgm_table <- renderDT({
    filtered_data_cross <- disposals_display[disposals_display$agency == input$cgm_agency &
                                               disposals_display$market_name %in% input$cgm_market,]

    if (input$cgm_best_odds) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}

    datatable(filtered_data_cross, selection = "multiple", filter = "top")
  }, server = FALSE)

  observeEvent(input$cgm_table_rows_selected,{
    output$cgm_selected <- renderDT({
      if(!is.null(input$cgm_table_rows_selected)){
        filtered_data_cross <- disposals_display[disposals_display$agency == input$cgm_agency &
                                                 disposals_display$market_name %in% input$cgm_market,]

        if (input$cgm_best_odds) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}

        selected_data_cross <- filtered_data_cross[input$cgm_table_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data_cross)
      }
    })
  })

  # Cross Game Comparison
  observeEvent(input$cgm_get_comparison, {
    # Get selected data
    filtered_data_cross <- disposals_display[disposals_display$agency == input$cgm_agency &
                                               disposals_display$market_name %in% input$cgm_market,]

    if (input$cgm_best_odds) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}

    selected_data_cross <- filtered_data_cross[input$cgm_table_rows_selected, c("player_name", "type", "line", "market_name", "price", "agency")]

    player_names_cross = selected_data_cross$player_name
    types_cross = selected_data_cross$type
    lines_cross = selected_data_cross$line
    market_names_cross = selected_data_cross$market_name

    # Call function
    comparison_df_cross <-
      compare_cgm(market_names_cross = market_names_cross,
                  player_names_cross = player_names_cross,
                  lines_cross = lines_cross,
                  types_cross = types_cross)

    # populate DTOutput
    output$cgm_odds_compare <- renderDT({
      datatable(comparison_df_cross)
    })
  })

  output$cgm_summary <- renderUI({
    if(!is.null(input$cgm_table_rows_selected)){
      filtered_data_cross <- disposals_display[disposals_display$agency == input$cgm_agency &
                                               disposals_display$market_name %in% input$cgm_market,]

      if (input$cgm_best_odds) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}

      selected_data_cross <- filtered_data_cross[input$cgm_table_rows_selected, ]
      uncorrelated_price_cross <- prod(selected_data_cross$price)
      empirical_price_cross <- 1 / prod(selected_data_cross$prob_2025)
      empirical_price_cross_l10 <- 1 / prod(selected_data_cross$prob_last_10)
      diff = 1/empirical_price_cross - 1/uncorrelated_price_cross
      diff_l10 = 1/empirical_price_cross_l10 - 1/uncorrelated_price_cross
      HTML(paste0("<strong>Multi Price:</strong>", " $", round(uncorrelated_price_cross, 2), "<br/>",
                  " <strong>Theoretical Multi Price:</strong>", " $", round(empirical_price_cross, 2), "<br/>",
                  " <strong>Edge L10:</strong>", " ", round(100*diff_l10, 3), "%"), "<br/>",
                  " <strong>Edge 2025:</strong>", " ", round(100*diff, 3), "%")
    }
  })
}

# ===============================================================================
# Run App
# ===============================================================================

shinyApp(ui, server)

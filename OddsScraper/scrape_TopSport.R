# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " ")) |> 
    mutate(player_full_name = paste(player_first_name, player_last_name, sep = " "))

# URL of website
topsport_url = "https://www.topsport.com.au/Sport/Basketball/NBL_Matches/Matches"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

# Get data from main market page
main_markets <-
topsport_url |> 
    read_html() |>
    html_nodes(".marketTable") |> 
    html_table()

#===============================================================================
# Use rvest to get additional market information-------------------------------#
#===============================================================================

# Get links to other markets
topsport_other_markets <-
    topsport_url |>
    read_html() |>
    html_nodes("dd") |> 
    html_attr("data-compurl")

# Remove NA
topsport_other_markets <- topsport_other_markets[!is.na(topsport_other_markets)]

# Remove ?issubcomp=true
topsport_other_markets <- str_remove(topsport_other_markets, "\\?issubcomp=true")

# Add base url
topsport_other_markets <- paste0("https://www.topsport.com.au", topsport_other_markets)

# Get only distinct URLs
topsport_other_markets <- unique(topsport_other_markets)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

head_to_head_main <- function() {

# Function to get head to head data--------------------------------------------#
get_h2h <- function(market_table) {
    
    # Home Team Data
    home_info <- market_table[2, 1:2]
    names(home_info) <- c("home_team", "home_win")
    
    # Away Team Data
    away_info <- market_table[3, 1:2]
    names(away_info) <- c("away_team", "away_win")
    
    # Match Start Time
    match_start_time <- market_table[1, 1]
    names(match_start_time) <- "start_time"
    
    # Combine together into one table
    bind_cols(home_info, away_info, match_start_time)
    
}

# Map function to main markets list
topsport_h2h <- map(main_markets, get_h2h) |> bind_rows()

# Fix names
topsport_h2h <-
topsport_h2h |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TopSport") |> 
    mutate(start_time = dmy_hm(start_time))

# Write to csv
write_csv(topsport_h2h, "Data/scraped_odds/topsport_h2h.csv")
}

player_props_main <- function() {

#===============================================================================
# Player Points
#===============================================================================

# Function to read the html of a given url
read_topsport_html <- function(url) {
    
    # Get market name from url
    market_name <- str_extract(url, "(?<=Basketball/).*")
    market_name <- str_remove(market_name, "\\/.*$")
    
    # Get line from market name
    line <- str_extract(market_name, "\\d+\\.?\\d?")
    
    # Get match name from html
    match_name_html <-    
        url |> 
        read_html() |>
        html_nodes("h1") |>
        html_text() |> 
        paste(collapse = " ")
    
    # Get match name from extracted text
    match_name_html <- strsplit(match_name_html, split = " - ")
    match_name <- match_name_html[[1]][length(match_name_html[[1]])]
    player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
    
    # Get data from html
    result <-    
    url |> 
        read_html() |>
        html_nodes(".marketTable") |> 
        html_table()
    
    # Get tibble
    result[[1]] |>
        mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.?\\d?"))) |>
        mutate(line = as.numeric(line)) |>
        mutate(match = match_name) |>
        mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player points--------------------------------------

# Get URLs
pick_your_own_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_(Score|Have)_.*_Points\\/")]

# Map function
player_points_alternate <-
map(pick_your_own_points_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_points_alternate) == 0) {
    player_points_alternate <-
        tibble(
        match = character(),
        Selection = character(),
        Win = numeric(),
        line = numeric()
    )
}

player_points_alternate <-
    player_points_alternate |> 
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    left_join(player_names_teams[c("player_full_name", "player_last_name", "player_first_name", "player_team")], by = c("Selection" = "player_full_name")) |> 
    rename(player_name = Selection) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |> 
    select(-player_first_name, -player_last_name) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team))

# Get data for player points over/under-----------------------------------------

# Get URLs
player_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Total_Points_.*\\(|Player_Points_\\-.*\\(")]

# Map function
player_points_lines <-
    map(player_points_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_points_lines) == 0) {
    player_points_lines <-
        tibble(
        match = character(),
        Selection = character(),
        Win = numeric(),
        line = numeric()
    )
}

player_points_lines <-
    player_points_lines |>
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    mutate(Selection = str_remove_all(Selection, "Upcoming Matches Total Points: ")) |>
    rename(over_price = Win)

# Get Overs
player_points_lines_overs <-
    player_points_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Over")) |>
    mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |>
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Get Unders
player_points_lines_unders <-
    player_points_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Under")) |>
        mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    rename(under_price = over_price) |>
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_points_lines <- 
    player_points_lines_overs |> 
    left_join(player_points_lines_unders) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(market_name = "Player Points") |> 
    mutate(agency = "TopSport")

#===============================================================================
# Player Assists
#===============================================================================

# Get data for pick your own player assists--------------------------------------

# Get URLs
pick_your_own_assists_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Have_.*_Assists")]

# Map function
player_assists_alternate <-
    map(pick_your_own_assists_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_assists_alternate) == 0) {
    player_assists_alternate <-
        tibble(
            match = character(),
            Selection = character(),
            Win = numeric(),
            line = numeric()
        )
}

player_assists_alternate <-
    player_assists_alternate |> 
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    left_join(player_names_teams[c("player_full_name", "player_last_name", "player_first_name", "player_team")], by = c("Selection" = "player_full_name")) |> 
    rename(player_name = Selection) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |> 
    select(-player_first_name, -player_last_name) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team))

# Get data for player assists over/under-----------------------------------------

# Get URLs
player_assists_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Total_Assists|Player_Assists")]

# Map function
player_assists_lines <-
    map(player_assists_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_assists_lines) == 0) {
    player_assists_lines <-
        tibble(
            match = character(),
            Selection = character(),
            Win = numeric(),
            line = numeric()
        )
}

player_assists_lines <-
    player_assists_lines |>
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    mutate(Selection = str_remove_all(Selection, "Upcoming Matches Total Assists: ")) |>
    rename(over_price = Win)

# Get Overs
player_assists_lines_overs <-
    player_assists_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Over")) |>
        mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Get Unders
player_assists_lines_unders <-
    player_assists_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Under")) |>
        mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
    rename(under_price = over_price) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_assists_lines <- 
    player_assists_lines_overs |> 
    left_join(player_assists_lines_unders) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(market_name = "Player Assists") |> 
    mutate(agency = "TopSport")

#===============================================================================
# Player Rebounds
#===============================================================================

# Get data for pick your own player rebounds--------------------------------------

# Get URLs
pick_your_own_rebounds_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Have_.*_Rebounds")]

# Map function
player_rebounds_alternate <-
    map(pick_your_own_rebounds_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_rebounds_alternate) == 0) {
    player_rebounds_alternate <-
        tibble(
            match = character(),
            Selection = character(),
            Win = numeric(),
            line = numeric()
        )
}

player_rebounds_alternate <-
    player_rebounds_alternate |> 
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    left_join(player_names_teams[c("player_full_name", "player_last_name", "player_first_name", "player_team")], by = c("Selection" = "player_full_name")) |> 
    rename(player_name = Selection) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |> 
    select(-player_first_name, -player_last_name) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team))

# Get data for player rebounds over/under-----------------------------------------

# Get URLs
player_rebounds_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Total_Rebounds|Player_Rebounds")]

# Map function
player_rebounds_lines <-
    map(player_rebounds_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_rebounds_lines) == 0) {
    player_rebounds_lines <-
        tibble(
            match = character(),
            Selection = character(),
            Win = numeric(),
            line = numeric()
        )
}

player_rebounds_lines <-
    player_rebounds_lines |>
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    mutate(Selection = str_remove_all(Selection, "Upcoming Matches Total Rebounds: ")) |>
    rename(over_price = Win)

# Get Overs
player_rebounds_lines_overs <-
    player_rebounds_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Over")) |>
        mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Get Unders
player_rebounds_lines_unders <-
    player_rebounds_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Under")) |>
        mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
    rename(under_price = over_price) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_rebounds_lines <- 
    player_rebounds_lines_overs |> 
    left_join(player_rebounds_lines_unders) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(market_name = "Player Rebounds") |> 
    mutate(agency = "TopSport")

#===============================================================================
# Player Threes
#===============================================================================

# Get data for pick your own player threes--------------------------------------

# Get URLs
pick_your_own_threes_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Have_.*_Threes")]

# Map function
player_threes_alternate <-
    map(pick_your_own_threes_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_threes_alternate) == 0) {
    player_threes_alternate <-
        tibble(
            match = character(),
            Selection = character(),
            Win = numeric(),
            line = numeric()
        )
}

player_threes_alternate <-
    player_threes_alternate |> 
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    left_join(player_names_teams[c("player_full_name", "player_last_name", "player_first_name", "player_team")], by = c("Selection" = "player_full_name")) |> 
    rename(player_name = Selection) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |> 
    select(-player_first_name, -player_last_name) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team))

# Get data for player threes over/under-----------------------------------------

# Get URLs
player_threes_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Total_Threes|Player_Threes")]

# Map function
player_threes_lines <-
    map(player_threes_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(player_threes_lines) == 0) {
    player_threes_lines <-
        tibble(
            match = character(),
            Selection = character(),
            Win = numeric(),
            line = numeric()
        )
}

player_threes_lines <-
    player_threes_lines |>
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(Selection = str_replace_all(Selection, "Trey Kell III", "Trey Kell")) |>
    mutate(Selection = str_replace_all(Selection, "Parker Jackson Cartwright", "Parker Jackson-Cartwright")) |>
    mutate(Selection = str_replace_all(Selection, "Derrick Walton", "Derrick Walton Jr")) |>
    mutate(Selection = str_replace_all(Selection, "Matthew Hurt", "Matt Hurt")) |>
    mutate(Selection = str_remove_all(Selection, " \\(.*\\)$")) |>
    mutate(Selection = str_remove_all(Selection, "Upcoming Matches Total Threes: ")) |>
    rename(over_price = Win)

# Get Overs
player_threes_lines_overs <-
    player_threes_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Over")) |>
    mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Get Unders
player_threes_lines_unders <-
    player_threes_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Under")) |>
    mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
    rename(under_price = over_price) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    mutate(line = as.numeric(line)) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_threes_lines <- 
    player_threes_lines_overs |> 
    left_join(player_threes_lines_unders) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(market_name = "Player Threes") |> 
    mutate(agency = "TopSport")

#===============================================================================
# Total Match Points
#===============================================================================

# Get data for total match points over/under-------------------------------------

# Get URLs
total_match_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "\\/Total_Points")]

# Map function
total_match_points_lines <-
    map(total_match_points_markets, read_topsport_html) |> 
    bind_rows()

if (nrow(total_match_points_lines) == 0) {
    total_match_points_lines <-
        tibble(
        match = character(),
        Selection = character(),
        Win = numeric(),
        line = numeric()
    )
}

total_match_points_lines_overs <-
    total_match_points_lines |> 
    filter(str_detect(Selection, "Over")) |>
    transmute(match, market_name = "Total Match Points", line, over_price = Win, agency = "TopSport")

total_match_points_lines_unders <-
    total_match_points_lines |> 
    filter(str_detect(Selection, "Under")) |>
    transmute(match, market_name = "Total Match Points", line, under_price = Win, agency = "TopSport")

total_match_points_lines <- 
    total_match_points_lines_overs |> 
    left_join(total_match_points_lines_unders) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(line = line/10)

#===============================================================================
# Write to CSV
#===============================================================================

# Points
player_points_lines |>
    bind_rows(player_points_alternate) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"
    ) |>
    mutate(market_name = "Player Points") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_points.csv")

# Rebounds
player_rebounds_lines |>
    bind_rows(player_rebounds_alternate) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"
    ) |>
    mutate(market_name = "Player Rebounds") |> 
    mutate(agency = "TopSport") |>
    write_csv("Data/scraped_odds/topsport_player_rebounds.csv")

# Assists
player_assists_lines |>
    bind_rows(player_assists_alternate) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"
    ) |>
    mutate(market_name = "Player Assists") |>
    mutate(agency = "TopSport") |>
    write_csv("Data/scraped_odds/topsport_player_assists.csv")

# Total Match Points
total_match_points_lines |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "line",
        "over_price",
        "under_price",
        "agency"
    ) |>
    mutate(agency = "TopSport") |>
    mutate(market_name = "Total Match Points") |>
    write_csv("Data/scraped_odds/topsport_total_match_points.csv")
}

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

h2h_safe_topsport <- safely(head_to_head_main)
player_props_safe_topsport <- safely(player_props_main)

# Run functions
h2h_safe_topsport()
player_props_safe_topsport()

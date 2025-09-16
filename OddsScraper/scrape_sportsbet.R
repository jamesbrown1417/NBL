# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

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
sportsbet_url = "https://www.sportsbet.com.au/betting/basketball-aus-other/australian-nbl"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {

# Get data from main market page
matches <-
    sportsbet_url |> 
    read_html_live() |>
    html_nodes(".White_fqa53j6")
    
# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".caption_f4zed5e") |>
        html_text()
    
    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
}

# Function to get odds
get_odds <- function(match) {
    odds <-
        match |>
        html_nodes(".priceTextSize_frw9zm9") |>
        html_text() |>
        as.numeric()
    
    # Home team
    home_win <- odds[1]
    away_win <- odds[2]
    
    # Output
    tibble(home_win, away_win)
}

# Function to get start time
get_start_time <- function(match) {
    start_time <-
        match |>
        html_nodes(".oneLine_f15ay66x") |>
        html_text()
    
    # Output
    tibble(start_time)
}

# Map functions to each match and combine together
all_main_market_data <-
    bind_cols(
        map(matches, get_team_names) |> bind_rows() |> filter(!is.na(home_team)),
        map(matches, get_odds) |> bind_rows() |> filter(!is.na(home_win)),
        map(matches, get_start_time) |> bind_rows() |> filter(!is.na(start_time))
    )

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

sportsbet_h2h <-
all_main_market_data |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet")

# Write to csv
write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")

}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {

# Get match links
match_links <-
sportsbet_url |> 
    read_html_live() |>
    html_nodes(".link_ft4u1lp") |> 
    html_attr("href")

# Get match IDs from links
match_ids <-
match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()

# Get Match Names from links
match_names <-
    match_links |>
    str_remove_all("/betting/basketball-aus-other/australian-nbl/") |>
    str_remove_all("-\\d{4,10}$") |>
    str_replace_all("-", " ") |>
    str_to_title()

# Table with match names and IDs
match_table_home <-
    tibble(match_names, match_ids) |> 
    filter(str_detect(match_names, " Vs ")) |>
    separate(match_names, into = c("home_team", "away_team"), sep = " Vs ", remove = TRUE) |> 
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    select(match, home_team, away_team, match_id = match_ids)

match_table_away <-
    tibble(match_names, match_ids) |> 
    filter(str_detect(match_names, " At ")) |>
    separate(match_names, into = c("home_team", "away_team"), sep = " At ", remove = TRUE) |> 
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    select(match, home_team, away_team, match_id = match_ids)

match_table <-
    bind_rows(match_table_home, match_table_away)

# Match info links
match_info_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/SportCard?displayWinnersPriceMkt=true&includeLiveMarketGroupings=true&includeCollection=true")

# Player points links
player_points_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/567/Markets")

# Player rebounds links
player_rebounds_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/568/Markets")

# Player assists links
player_assists_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/569/Markets")

# Player threes links
player_threes_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/710/Markets")

# Player PRA links
player_pra_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/782/Markets")

# Get IDs needed for SGM engine-------------------------------------------------
read_prop_url_metadata <- function(url) {
    
    # Make request and get response
    sb_response <-
        request(url) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |> 
        req_headers("Referer" = "https://www.sportsbet.com.au") |>
        req_perform() |> 
        resp_body_json()
    
    # Empty vectors to append to
    class_external_id = c()
    competition_external_id = c()
    event_external_id = c()
    
    # Append to vectors
    class_external_id = c(class_external_id, sb_response$classExternalId)
    competition_external_id = c(competition_external_id, sb_response$competitionExternalId)
    event_external_id = c(event_external_id, sb_response$externalId)
    
    # Output
    tibble(class_external_id,
           competition_external_id,
           event_external_id,
           url) |> 
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
        rename(match_id = url) |> 
        mutate(match_id = as.numeric(match_id))
}

# Safe version that just returns NULL if there is an error
safe_read_prop_metadata <- safely(read_prop_url_metadata, otherwise = NULL)

# Map function to player points urls
player_prop_metadata <-
    map(match_info_links, safe_read_prop_metadata)

# Get just result part from output
player_prop_metadata <-
    player_prop_metadata |>
    map("result") |>
    map_df(bind_rows)

# Function to read a url and get the player props-------------------------------

read_prop_url <- function(url) {
    
    # Make request and get response
    sb_response <-
        request(url) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |> 
        req_headers("Referer" = "https://www.sportsbet.com.au") |>
        req_perform() |> 
        resp_body_json()
    
    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()
    
    # Loop through each market
    for (market in sb_response) {
        for (selection in market$selections) {
            
            # Append to vectors
            prop_market_name = c(prop_market_name, market$name)
            selection_name_prop = c(selection_name_prop, selection$name)
            prop_market_selection = c(prop_market_selection, selection$resultType)
            prop_market_price = c(prop_market_price, selection$price$winPrice)
            player_id = c(player_id, selection$externalId)
            market_id = c(market_id, market$externalId)
            if (is.null(selection$unformattedHandicap)) {
                selection$unformattedHandicap = NA
                handicap = c(handicap, selection$unformattedHandicap)
            } else {
                selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
                handicap = c(handicap, selection$unformattedHandicap)
            }
        }
    }
    
    # Output
    tibble(prop_market_name,
           selection_name_prop,
           prop_market_selection,
           prop_market_price,
           player_id,
           market_id,
           handicap,
           url)
}

# Safe version that just returns NULL if there is an error
safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)

#===============================================================================
# Player Points
#===============================================================================

# Map function to player points urls
player_points_data <-
    map(player_points_links, safe_read_prop_url)

# Get just result part from output
player_points_data <-
    player_points_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name
player_points_data <-
    player_points_data |>
    mutate(market_name = "Player Points") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "^Mitch ", "Mitchell ")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "^Mitch ", "Mitchell ")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Mcveigh", "McVeigh")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Mcveigh", "McVeigh")) |> 
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Matthew Mooney", "Matt Mooney")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "William McDowell White", "Will McDowell-White")) |> 
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "William McDowell White", "Will McDowell-White")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Le Afa", "Le'Afa")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Le Afa", "Le'Afa")) |> 
    left_join(player_prop_metadata)

# Get player points alternate lines---------------------------------------------

player_points_alternate <-
    player_points_data |> 
    filter(str_detect(prop_market_name, "To Score")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id)

# Get player points over / under -----------------------------------------------

player_points_over <-
    player_points_data |> 
    filter(str_detect(selection_name_prop, "Over")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Over")) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id )
 
player_points_under <-
    player_points_data |> 
    filter(str_detect(selection_name_prop, "Under")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Under")) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(match_table) |>
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id_unders = player_id
    )

# Combine
player_points_over_under <-
    player_points_over |> 
    left_join(player_points_under)

#===============================================================================
# Player Assists
#===============================================================================

# Map function to player assists urls
player_assists_data <-
    map(player_assists_links, safe_read_prop_url)

# Get just result part from output
player_assists_data <-
    player_assists_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name
player_assists_data <-
    player_assists_data |>
    mutate(market_name = "Player Assists") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "^Mitch ", "Mitchell ")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "^Mitch ", "Mitchell ")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Mcveigh", "McVeigh")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Mcveigh", "McVeigh")) |> 
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Matthew Mooney", "Matt Mooney")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "William McDowell White", "Will McDowell-White")) |> 
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "William McDowell White", "Will McDowell-White")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Le Afa", "Le'Afa")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Le Afa", "Le'Afa")) |> 
    left_join(player_prop_metadata)

# Get player assists alternate lines---------------------------------------------
player_assists_alternate <-
    player_assists_data |> 
    filter(str_detect(prop_market_name, "To Record")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id )

# Get player assists over / under -----------------------------------------------

player_assists_over <-
    player_assists_data |> 
    filter(str_detect(selection_name_prop, "Over")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Over")) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id )

player_assists_under <-
    player_assists_data |> 
    filter(str_detect(selection_name_prop, "Under")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Under")) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id_unders = player_id
    )

# Combine
player_assists_over_under <-
    player_assists_over |> 
    left_join(player_assists_under)
 
#===============================================================================
# Player Rebounds
#===============================================================================

# Map function to player rebounds urls
player_rebounds_data <-
    map(player_rebounds_links, safe_read_prop_url)

# Get just result part from output
player_rebounds_data <-
    player_rebounds_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name
player_rebounds_data <-
    player_rebounds_data |>
    mutate(market_name = "Player Rebounds") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "^Mitch ", "Mitchell ")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "^Mitch ", "Mitchell ")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Mcveigh", "McVeigh")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Mcveigh", "McVeigh")) |> 
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Matthew Mooney", "Matt Mooney")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "William McDowell White", "Will McDowell-White")) |> 
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "William McDowell White", "Will McDowell-White")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Le Afa", "Le'Afa")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Le Afa", "Le'Afa")) |> 
    left_join(player_prop_metadata)

# Get player rebounds alternate lines---------------------------------------------
player_rebounds_alternate <-
    player_rebounds_data |> 
    filter(str_detect(prop_market_name, "To Record")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id )

# Get player rebounds over / under -----------------------------------------------

player_rebounds_over <-
    player_rebounds_data |> 
    filter(str_detect(selection_name_prop, "Over")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Over")) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id )

player_rebounds_under <-
    player_rebounds_data |> 
    filter(str_detect(selection_name_prop, "Under")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Under")) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id_unders = player_id
    )

# Combine
player_rebounds_over_under <-
    player_rebounds_over |> 
    left_join(player_rebounds_under)

#===============================================================================
# Player Threes
#===============================================================================

# Map function to player threes urls
player_threes_data <-
    map(player_threes_links, safe_read_prop_url)

# Get just result part from output
player_threes_data <-
    player_threes_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name
player_threes_data <-
    player_threes_data |>
    mutate(market_name = "Player Threes") |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "^Mitch ", "Mitchell ")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "^Mitch ", "Mitchell ")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Mcveigh", "McVeigh")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Matthew Mooney", "Matt Mooney")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "William McDowell White", "Will McDowell-White")) |> 
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "William McDowell White", "Will McDowell-White")) |> 
    mutate(prop_market_name = str_replace_all(prop_market_name, "Le Afa", "Le'Afa")) |>
    mutate(selection_name_prop = str_replace_all(selection_name_prop, "Le Afa", "Le'Afa")) |> 
    left_join(player_prop_metadata)

# Get player threes alternate lines---------------------------------------------
player_threes_alternate <-
    player_threes_data |> 
    filter(str_detect(prop_market_name, "\\+ Made Threes$")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id)

# Get player threes over / under -----------------------------------------------

player_threes_over <-
    player_threes_data |> 
    filter(str_detect(selection_name_prop, "Over")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Over")) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id )

player_threes_under <-
    player_threes_data |> 
    filter(str_detect(selection_name_prop, "Under")) |> 
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Under")) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id_unders = player_id
    )

# Combine
player_threes_over_under <-
    player_threes_over |> 
    left_join(player_threes_under)

#===============================================================================
# Write to CSV
#===============================================================================

# Points
player_points_alternate |>
    bind_rows(player_points_over_under) |>
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
        "opposition_team",
        "class_external_id",
        "competition_external_id",
        "event_external_id",
        "market_id",
        "player_id",
        "player_id_unders"
    ) |>
    mutate(market_name = "Player Points") |>
    mutate(agency = "Sportsbet") |> 
    write_csv("Data/scraped_odds/sportsbet_player_points.csv")

# Rebounds
player_rebounds_alternate |>
    bind_rows(player_rebounds_over_under) |>
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
        "opposition_team",
        "class_external_id",
        "competition_external_id",
        "event_external_id",
        "market_id",
        "player_id",
        "player_id_unders"
    ) |>
    mutate(market_name = "Player Rebounds") |>
    mutate(agency = "Sportsbet") |> 
    write_csv("Data/scraped_odds/sportsbet_player_rebounds.csv")

# Assists
player_assists_alternate |>
    bind_rows(player_assists_over_under) |>
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
        "opposition_team",
        "class_external_id",
        "competition_external_id",
        "event_external_id",
        "market_id",
        "player_id",
        "player_id_unders"
    ) |>
    mutate(market_name = "Player Assists") |>
    mutate(agency = "Sportsbet") |> 
    write_csv("Data/scraped_odds/sportsbet_player_assists.csv")

# Threes
player_threes_alternate |>
    bind_rows(player_threes_over_under) |>
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
        "opposition_team",
        "class_external_id",
        "competition_external_id",
        "event_external_id",
        "market_id",
        "player_id",
        "player_id_unders"
    ) |>
    mutate(market_name = "Player Threes") |>
    mutate(agency = "Sportsbet") |> 
    write_csv("Data/scraped_odds/sportsbet_player_threes.csv")
}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()

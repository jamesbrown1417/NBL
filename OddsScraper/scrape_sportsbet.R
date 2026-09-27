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
    read_csv(data_file("raw_stats", "supercoach-data.csv")) |>
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " ")) |> 
    mutate(player_full_name = fix_player_names(paste(player_first_name, player_last_name, sep = " "))) |>
    distinct(player_full_name, player_team, .keep_all = TRUE)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/basketball-aus-other/australian-nbl"

# Parse each event as a unit: Sportsbet displays away @ home for NBL games.
parse_sportsbet_matches <- function(page) {
    cards <- html_elements(page, '[data-automation-id$="-competition-event-card"]')
    if (!length(cards)) stop("Sportsbet: no event cards found; check page access/markup.")
    map_dfr(cards, function(card) {
        link <- html_attr(html_element(card, 'a[href*="/australian-nbl/"]'), "href")
        parts <- str_match(link, "/australian-nbl/(.+)-(at|vs|v)-(.+)-([0-9]+)$")
        if (is.na(parts[1, 1])) stop("Sportsbet: unrecognised event link: ", link)
        teams <- html_elements(card, '[data-automation-id$="-two-outcome-captioned-label"]') |> html_text2()
        prices <- html_elements(card, '[data-automation-id$="-two-outcome-captioned-text"]') |> html_text2() |> as.numeric()
        if (length(teams) != 2L || length(prices) != 2L || anyNA(prices) || any(prices <= 1)) {
            stop("Sportsbet: missing or invalid head-to-head prices for ", link)
        }
        home <- if (parts[1, 3] == "at") 2L else 1L
        away <- 3L - home
        tibble(match_id = as.numeric(parts[1, 5]),
               home_team = fix_team_names(teams[home]),
               away_team = fix_team_names(teams[away]),
               home_win = prices[home], away_win = prices[away])
    }) |>
        distinct(match_id, .keep_all = TRUE) |>
        mutate(match = paste(home_team, "v", away_team))
}

main_markets_function <- function(matches) {
    sportsbet_h2h <- matches |>
        transmute(match, market_name = "Head To Head", home_team, home_win,
                  away_team, away_win,
                  margin = round(1 / home_win + 1 / away_win, 3), agency = "Sportsbet")
    write_csv(sportsbet_h2h, data_file("raw_odds", "sportsbet_h2h.csv"))
}

# Do not carry a previous-season team into a new fixture.
validate_sportsbet_roster <- function(data) {
    invalid <- is.na(data$player_team) |
        !(data$player_team == data$home_team | data$player_team == data$away_team)
    if (any(invalid)) {
        warning("Sportsbet roster needs updating: ",
                paste(sort(unique(data$player_name[invalid])), collapse = ", "),
                call. = FALSE)
        data$player_team[invalid] <- NA_character_
        data$opposition_team[invalid] <- NA_character_
    }
    data
}

write_sportsbet_props <- function(data, file) {
    write_csv(validate_sportsbet_roster(data), file)
}

player_props_function <- function(matches) {
match_table <- matches |> select(match, home_team, away_team, match_id)
match_ids <- match_table$match_id

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

# Player Defensive Props
player_defensive_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/1097/Markets")

# Get IDs needed for SGM engine-------------------------------------------------
available_prop_urls <- character()
read_prop_url_metadata <- function(url) {
    
    # Make request and get response
    sb_response <-
        request(url) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |> 
        req_headers("Referer" = "https://www.sportsbet.com.au") |>
        req_timeout(30) |>
        req_perform() |> 
        resp_body_json()
    
    event_id <- str_match(url, "/Events/([0-9]+)/")[, 2]
    available_prop_urls <<- c(available_prop_urls, vapply(sb_response$marketGrouping,
        function(group) paste0("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/",
                               event_id, "/MarketGroupings/", group$id, "/Markets"), character(1)))

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
        mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
        rename(match_id = url) |> 
        mutate(match_id = as.numeric(match_id))
}

# Propagate request errors so a partial scrape cannot appear successful
safe_read_prop_metadata <- function(url) list(result = read_prop_url_metadata(url))

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
    
    # An unadvertised group is unavailable, not a failed HTTP request.
    sb_response <- list()
    if (url %in% available_prop_urls) {
        sb_response <- request(url) |>
            req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |>
            req_headers("Referer" = "https://www.sportsbet.com.au") |>
            req_timeout(30) |>
            req_perform() |>
            resp_body_json()
    } else {
        message("Sportsbet: market group not offered: ", url)
    }

    # Empty vectors to append to
    prop_market_name = character()
    selection_name_prop = character()
    prop_market_selection = character()
    prop_market_price = numeric()
    player_id = numeric()
    market_id = numeric()
    handicap = numeric()
    
    # Loop through each market
    for (market in sb_response) {
        if (!identical(market$statusCode, "A")) next
        for (selection in market$selections) {
            if (!identical(selection$statusCode, "A") || is.null(selection$price$winPrice)) next
            
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
           url = rep(url, length(prop_market_name)))
}

# Propagate request errors so a partial scrape cannot appear successful
safe_read_prop_url <- function(url) list(result = read_prop_url(url))

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
    mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = fix_player_names(prop_market_name)) |>
    mutate(selection_name_prop = fix_player_names(selection_name_prop)) |> 
    left_join(player_prop_metadata)

# Get player points alternate lines---------------------------------------------

player_points_alternate <-
    player_points_data |> 
    filter(str_detect(prop_market_name, "To Score")) |>
    filter(str_detect(prop_market_name, "Qtr|Quarter", negate = TRUE)) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = fix_player_names(player_name)) |>
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    filter(str_detect(prop_market_name, "Qtr|Quarter", negate = TRUE)) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Over")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    filter(str_detect(prop_market_name, "Qtr|Quarter", negate = TRUE)) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = str_remove(player_name, " Under")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(match_table) |>
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = fix_player_names(prop_market_name)) |>
    mutate(selection_name_prop = fix_player_names(selection_name_prop)) |> 
    left_join(player_prop_metadata)

# Get player assists alternate lines---------------------------------------------
player_assists_alternate <-
    player_assists_data |> 
    filter(str_detect(prop_market_name, "To Record")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = fix_player_names(player_name)) |>
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = fix_player_names(prop_market_name)) |>
    mutate(selection_name_prop = fix_player_names(selection_name_prop)) |> 
    left_join(player_prop_metadata)

# Get player rebounds alternate lines---------------------------------------------
player_rebounds_alternate <-
    player_rebounds_data |> 
    filter(str_detect(prop_market_name, "To Record")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = fix_player_names(player_name)) |>
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    mutate(prop_market_name = fix_player_names(prop_market_name)) |>
    mutate(selection_name_prop = fix_player_names(selection_name_prop)) |> 
    left_join(player_prop_metadata)

# Get player threes alternate lines---------------------------------------------
player_threes_alternate <-
    player_threes_data |> 
    filter(str_detect(prop_market_name, "\\+ Made Threes$")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |> 
    mutate(player_name = fix_player_names(player_name)) |>
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(over_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |> 
    rename(under_price = prop_market_price) |> 
    left_join(match_table) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    relocate(match, .before = player_name) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
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
# Player PRAs
#===============================================================================

# Map function to player PRA urls
player_pras_data <-
    map(player_pra_links, safe_read_prop_url)

# Get just result part from output
player_pras_data <-
    player_pras_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name and join metadata
player_pras_data <-
    player_pras_data |>
    mutate(market_name = "Player PRAs") |>
    mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    mutate(prop_market_name = fix_player_names(prop_market_name)) |>
    mutate(selection_name_prop = fix_player_names(selection_name_prop)) |>
    left_join(player_prop_metadata)

# Get PRA alternate lines (e.g., "To Record 25+ ...")
player_pras_alternate <-
    player_pras_data |>
    filter(str_detect(prop_market_name, "To Record")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,3}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(over_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player PRAs",
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

# Get PRA over / under
player_pras_over <-
    player_pras_data |>
    filter(str_detect(selection_name_prop, "Over")) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Over")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |>
    rename(over_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player PRAs",
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

player_pras_under <-
    player_pras_data |>
    filter(str_detect(selection_name_prop, "Under")) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Under")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |>
    rename(under_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player PRAs",
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
player_pras_over_under <-
    player_pras_over |>
    left_join(player_pras_under)

#===============================================================================
# Player Steals
#===============================================================================

# Map function to player Defensive Props urls
player_defensive_props_data <-
    map(player_defensive_links, safe_read_prop_url)

# Get just result part from output
player_steals_data <-
    player_defensive_props_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name and join metadata
player_steals_data <-
    player_steals_data |>
    mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    mutate(prop_market_name = fix_player_names(prop_market_name)) |>
    mutate(selection_name_prop = fix_player_names(selection_name_prop)) |>
    left_join(player_prop_metadata)

# Filter to steals markets only
player_steals_data <-
    player_steals_data |>
    filter(str_detect(prop_market_name, "Steal"))

# Get steals alternate lines (e.g., "To Record 2+ Steals")
player_steals_alternate <-
    player_steals_data |>
    filter(str_detect(prop_market_name, "To Record")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(over_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Steals",
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

# Get steals over / under
player_steals_over <-
    player_steals_data |>
    filter(str_detect(selection_name_prop, "Over")) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Over")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |>
    rename(over_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Steals",
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

player_steals_under <-
    player_steals_data |>
    filter(str_detect(selection_name_prop, "Under")) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Under")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |>
    rename(under_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Steals",
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
player_steals_over_under <-
    player_steals_over |>
    left_join(player_steals_under)

#===============================================================================
# Player Blocks
#===============================================================================

# Get just result part from output
player_blocks_data <-
    player_defensive_props_data |>
    map("result") |>
    map_df(bind_rows)

# Add market name and join metadata
player_blocks_data <-
    player_blocks_data |>
    mutate(url = str_match(as.character(url), "/Events/([0-9]+)/")[, 2]) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    mutate(prop_market_name = fix_player_names(prop_market_name)) |>
    mutate(selection_name_prop = fix_player_names(selection_name_prop)) |>
    left_join(player_prop_metadata)

# Filter to blocks markets only
player_blocks_data <-
    player_blocks_data |>
    filter(str_detect(prop_market_name, "Block"))

# Get blocks alternate lines (e.g., "To Record 2+ Blocks")
player_blocks_alternate <-
    player_blocks_data |>
    filter(str_detect(prop_market_name, "To Record")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(over_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Blocks",
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

# Get blocks over / under
player_blocks_over <-
    player_blocks_data |>
    filter(str_detect(selection_name_prop, "Over")) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Over")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |>
    rename(over_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Blocks",
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

player_blocks_under <-
    player_blocks_data |>
    filter(str_detect(selection_name_prop, "Under")) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Under")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    rename(line = handicap) |>
    rename(under_price = prop_market_price) |>
    left_join(match_table) |>
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |>
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                              player_team == away_team ~ home_team,
                                              TRUE ~ NA_character_)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Blocks",
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
player_blocks_over_under <-
    player_blocks_over |>
    left_join(player_blocks_under)

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
    write_sportsbet_props(data_file("raw_odds", "sportsbet_player_points.csv"))

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
    write_sportsbet_props(data_file("raw_odds", "sportsbet_player_rebounds.csv"))

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
    write_sportsbet_props(data_file("raw_odds", "sportsbet_player_assists.csv"))

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
    write_sportsbet_props(data_file("raw_odds", "sportsbet_player_threes.csv"))

# PRAs
player_pras_alternate |>
    bind_rows(player_pras_over_under) |>
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
    mutate(market_name = "Player PRAs") |>
    mutate(agency = "Sportsbet") |>
    write_sportsbet_props(data_file("raw_odds", "sportsbet_player_pras.csv"))

# Steals
player_steals_alternate |>
    bind_rows(player_steals_over_under) |>
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
    mutate(market_name = "Player Steals") |>
    mutate(agency = "Sportsbet") |>
    write_sportsbet_props(data_file("raw_odds", "sportsbet_player_steals.csv"))

# Blocks
player_blocks_alternate |>
    bind_rows(player_blocks_over_under) |>
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
    mutate(market_name = "Player Blocks") |>
    mutate(agency = "Sportsbet") |>
    write_sportsbet_props(data_file("raw_odds", "sportsbet_player_blocks.csv"))

}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

# A single page snapshot keeps head-to-head and prop match orientation aligned.
# Tests can load the parsers without making requests or writing odds.
if (!isTRUE(getOption("nbl.sportsbet.skip_run", FALSE))) {
    sportsbet_matches <- parse_sportsbet_matches(read_html_live(sportsbet_url))
    player_props_function(sportsbet_matches)
    main_markets_function(sportsbet_matches)
    message("Sportsbet: refreshed ", nrow(sportsbet_matches), " matches.")
}

# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(httr)
library(jsonlite)

# Load user functions
source("Scripts/04-helper-functions.R")

# Keep parsing callable without reading files or launching a scrape.
tab_market_rows <- function(response) {
    if (!is.list(response) || !("matches" %in% names(response)) || !is.list(response$matches)) {
        stop("TAB: expected a competition response with a matches list.")
    }
    rows <- tibble(match = character(), start_time = character(), market_name = character(),
                   prop_id = character(), prop_name = character(), price = numeric(),
                   position = character())
    for (event in response$matches) {
        if (is.null(event$name) || is.null(event$startTime) || is.null(event$markets)) {
            stop("TAB: incomplete match record.")
        }
        for (market in event$markets) {
            if (!identical(market$bettingStatus, "Open") || identical(market$onlineBetting, FALSE)) next
            for (prop in market$propositions) {
                if (!identical(prop$bettingStatus, "Open") || identical(prop$isOpen, FALSE)) next
                if (is.null(prop$returnWin) || !is.finite(prop$returnWin) || prop$returnWin <= 1) next
                if (is.null(prop$id) || is.null(prop$name) || is.null(market$betOption)) {
                    stop("TAB: incomplete open proposition.")
                }
                rows <- bind_rows(rows, tibble(match = event$name, start_time = event$startTime,
                    market_name = market$betOption, prop_id = as.character(prop$id),
                    prop_name = prop$name, price = as.numeric(prop$returnWin),
                    position = prop$position %||% NA_character_))
            }
        }
    }
    rows
}

tab_name_key <- function(name) {
    name |> str_replace_all("\\bJ-Cartwright\\b", "Jackson-Cartwright") |>
        fix_player_names() |> str_replace_all("Jr(?:\\s+Jr)+", "Jr") |>
        str_to_lower() |> str_replace_all("[^\\p{L}0-9]", "")
}

resolve_tab_players <- function(data, roster) {
    full_names <- fix_player_names(paste(roster$player_first_name, roster$player_last_name))
    initials <- paste(str_sub(roster$player_first_name, 1, 1), roster$player_last_name)
    keys <- list(tab_name_key(full_names), tab_name_key(initials), tab_name_key(roster$player_last_name))
    data$player_team <- rep(NA_character_, nrow(data))
    for (i in seq_len(nrow(data))) {
        key <- tab_name_key(data$player_name[i])
        eligible <- roster$player_team %in% c(data$home_team[i], data$away_team[i])
        # Prefer exact full names, then initials, then an unambiguous surname.
        for (pool in keys) {
            hit <- which(eligible & !is.na(key) & pool == key)
            if (length(hit) == 1L) {
                data$player_name[i] <- full_names[hit]
                data$player_team[i] <- roster$player_team[hit]
                break
            }
            if (length(hit) > 1L) break
        }
    }
    if (anyNA(data$player_team)) {
        warning("TAB roster names unresolved: ", paste(unique(data$player_name[is.na(data$player_team)]), collapse = ", "), call. = FALSE)
    }
    data |> mutate(opposition_team = case_when(
        player_team == home_team ~ away_team,
        player_team == away_team ~ home_team,
        TRUE ~ NA_character_)) |> relocate(player_name, player_team, .after = market_name)
}

main_tab <- function(response_path = data_file("raw_odds", "responses/tab/tab_response.json"),
                     output_dir = data_paths$raw_odds,
                     roster = read_csv(data_file("raw_stats", "supercoach-data.csv"), show_col_types = FALSE),
                     max_age_seconds = 1800) {
    age <- as.numeric(difftime(Sys.time(), file.info(response_path)$mtime, units = "secs"))
    if (is.na(age) || age > max_age_seconds) {
        stop("TAB: response file is missing or stale; run get-TAB-response.py first.")
    }
    tab_response <- fromJSON(response_path, simplifyVector = FALSE)
    all_tab_markets <- tab_market_rows(tab_response)

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    filter(fix_team_names(prop_name) == fix_team_names(home_team)) |>
    rename(home_win = price) |> 
    select(-prop_name) |> 
    rename(home_prop_id = prop_id)

# Away teams
away_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    filter(fix_team_names(prop_name) == fix_team_names(away_team)) |>
    rename(away_win = price) |> 
    select(-prop_name) |> 
    rename(away_prop_id = prop_id)

# Combine
tab_head_to_head_markets <-
    home_teams |>
    left_join(away_teams, by = c("match", "home_team", "away_team", "start_time", "market_name")) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
    tab_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv


#===============================================================================
# Total line markets
#===============================================================================

# Under lines
under_lines <-
    all_tab_markets |>
    filter(market_name == "Total Points Over/Under") |> 
    filter(str_detect(prop_name, "Under")) |> 
    mutate(line = as.numeric(str_extract(prop_name, "[0-9]+(?:\\.[0-9]+)?"))) |>
    select(match, start_time, market_name, line, under_price = price, under_prop_id = prop_id)

# Over lines
over_lines <-
    all_tab_markets |>
    filter(market_name == "Total Points Over/Under") |> 
    filter(str_detect(prop_name, "Over")) |> 
    mutate(line = as.numeric(str_extract(prop_name, "[0-9]+(?:\\.[0-9]+)?"))) |>
    select(match, start_time, market_name, line, over_price = price, prop_id)

# Combine
tab_total_line_markets <-
    under_lines |>
    left_join(over_lines, by = c("match", "start_time", "market_name", "line")) |>
    select(match, start_time, market_name, line, under_price, over_price) |> 
    mutate(margin = round((1/under_price + 1/over_price), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_total_line_markets <-
    tab_total_line_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market = "Total Match Points")

# Write to csv


#===============================================================================
# Player Points
#===============================================================================

# Filter to player points markets
player_points_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Points$)|(\\d+\\+ Points)"))

# Extract player names
player_points_markets <-
    player_points_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Points"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Points"), str_replace(prop_name, "Points", "Pts") , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9]+(?:\\.[0-9]+)?")) |>
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Points", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Points"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(match, prop_name) |>
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_points_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_points_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_points_markets <-
    over_lines |>
    full_join(under_lines, by = c("match", "market_name", "player_name", "line")) |>
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_points_markets <-
    tab_player_points_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

tab_player_points_markets <- resolve_tab_players(tab_player_points_markets, roster)

#===============================================================================
# Player Assists
#===============================================================================

# Filter to player assists markets
player_assists_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Assists$)|(\\d+\\+ Assists)"))

# Extract player names
player_assists_markets <-
    player_assists_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Assists"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Assists"), str_replace(prop_name, "Assists", "Ast") , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9]+(?:\\.[0-9]+)?")) |>
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Assists", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Assists"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(match, prop_name) |>
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_assists_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_assists_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_assists_markets <-
    over_lines |>
    full_join(under_lines, by = c("match", "market_name", "player_name", "line")) |>
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_assists_markets <-
    tab_player_assists_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

tab_player_assists_markets <- resolve_tab_players(tab_player_assists_markets, roster)

#===============================================================================
# Player Rebounds
#===============================================================================

# Filter to player rebounds markets
player_rebounds_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Rebounds$)|(\\d+\\+ Rebounds)"))

# Extract player names
player_rebounds_markets <-
    player_rebounds_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Rebounds"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Rebounds"), str_replace(prop_name, "Rebounds", "Reb") , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9]+(?:\\.[0-9]+)?")) |>
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Rebounds", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Rebounds"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(match, prop_name) |>
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_rebounds_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_rebounds_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_rebounds_markets <-
    over_lines |>
    full_join(under_lines, by = c("match", "market_name", "player_name", "line")) |>
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_rebounds_markets <-
    tab_player_rebounds_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

tab_player_rebounds_markets <- resolve_tab_players(tab_player_rebounds_markets, roster)

#===============================================================================
# Player Threes
#=============================================================================== 

# Filter to player threes markets
player_threes_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Threes$)|(\\d+\\+ Threes)"))

# Extract player names
player_threes_markets <-
    player_threes_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Threes"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9]+(?:\\.[0-9]+)?")) |>
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Threes", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Threes"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(match, prop_name) |>
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_threes_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Threes") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_threes_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Threes") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_threes_markets <-
    over_lines |>
    full_join(under_lines, by = c("match", "market_name", "player_name", "line")) |>
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_threes_markets <-
    tab_player_threes_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

tab_player_threes_markets <- resolve_tab_players(tab_player_threes_markets, roster)

#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

outputs <- list(tab_h2h = tab_head_to_head_markets,
                tab_total_points = tab_total_line_markets,
                tab_player_points = tab_player_points_markets,
                tab_player_assists = tab_player_assists_markets,
                tab_player_rebounds = tab_player_rebounds_markets,
                tab_player_threes = tab_player_threes_markets)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
iwalk(outputs, ~ write_csv(.x, file.path(output_dir, paste0(.y, ".csv"))))
message("TAB: wrote ", sum(map_int(outputs, nrow)), " rows across ", length(outputs), " files.")
invisible(outputs)
}

if (!isTRUE(getOption("nbl.tab.skip_run", FALSE))) main_tab()

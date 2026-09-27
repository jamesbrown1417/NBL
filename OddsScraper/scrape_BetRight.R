# BetRight NBL odds. The six CSV schemas and SGM identifiers are preserved.
library(tidyverse)
library(httr2)
source("Scripts/04-helper-functions.R")

betright_get_json <- function(url) {
    request(url) |> req_timeout(30) |> req_perform() |> resp_body_json()
}

betright_matches <- function(response) {
    if (!is.list(response) || !is.list(response$masterCategories)) {
        stop("BetRight: expected masterCategories in category response.")
    }
    result <- tibble(match_id = character(), match = character(), start_time = character(),
                     home_team = character(), away_team = character())
    for (master in response$masterCategories) {
        if (!is.list(master$categories)) stop("BetRight: missing categories.")
        for (category in master$categories) {
            if (!identical(as.character(category$categoryId), "110")) next
            if (!is.list(category$masterEvents)) stop("BetRight: missing masterEvents.")
            for (event in category$masterEvents) {
                if (!identical(event$masterEventClassName, "Matches")) next
                if (is.null(event$masterEventId) || is.null(event$masterEventName) ||
                    is.null(event$minAdvertisedStartTimeUtc)) stop("BetRight: incomplete fixture.")
                teams <- str_split(event$masterEventName, fixed(" v "))[[1]]
                if (length(teams) != 2L) stop("BetRight: unrecognised fixture: ", event$masterEventName)
                home <- fix_team_names(teams[1])
                away <- fix_team_names(teams[2])
                result <- bind_rows(result, tibble(
                    match_id = as.character(event$masterEventId), match = paste(home, "v", away),
                    start_time = event$minAdvertisedStartTimeUtc, home_team = home, away_team = away))
            }
        }
    }
    distinct(result, match_id, .keep_all = TRUE)
}

betright_empty_rows <- function() {
    tibble(match_id = character(), event_name = character(), event_class = character(),
           event_id = character(), outcome_name = character(), outcome_id = character(),
           fixed_market_id = character(), price = numeric())
}

parse_betright_event <- function(response, match_id) {
    if (!is.list(response) || !is.list(response$events) ||
        !identical(as.character(response$masterEvent$masterEventId), as.character(match_id))) {
        stop("BetRight: invalid event response or fixture ID mismatch.")
    }
    rows <- betright_empty_rows()
    for (event in response$events) {
        if (is.null(event$eventName) || is.null(event$eventId) || !is.list(event$outcomes)) {
            stop("BetRight: incomplete market response.")
        }
        for (outcome in event$outcomes) {
            if (is.null(outcome$price) || !is.finite(outcome$price) || outcome$price <= 1) next
            if (!identical(outcome$marketTypeCode, "WIN")) next
            if (is.null(outcome$outcomeName) || is.null(outcome$outcomeId) || is.null(outcome$fixedMarketId)) {
                stop("BetRight: priced selection is missing its name or SGM identifiers.")
            }
            rows <- bind_rows(rows, tibble(
                match_id = as.character(match_id), event_name = event$eventName,
                event_class = event$eventClass %||% NA_character_, event_id = as.character(event$eventId),
                outcome_name = outcome$outcomeName, outcome_id = as.character(outcome$outcomeId),
                fixed_market_id = as.character(outcome$fixedMarketId), price = as.numeric(outcome$price)))
        }
    }
    rows
}

betright_player_market <- function(rows, stat, roster) {
    patterns <- c(points = "^Player Points - ", rebounds = "^Player Rebounds - ",
                  assists = "^Player Assists - ",
                  pras = "^Player Points (?:&|\\+) Assists (?:&|\\+) Rebounds - ",
                  threes = "^(?:Player Three Pointers|Threes Made|Player Threes) - ")
    labels <- c(points = "Player Points", rebounds = "Player Rebounds", assists = "Player Assists",
                pras = "Player PRAs", threes = "Player Threes")
    data <- rows |> filter(str_detect(event_name, patterns[[stat]])) |>
        mutate(player_name = str_remove(event_name, patterns[[stat]]) |>
                   str_remove("\\s+\\([^)]*\\)$") |> str_squish() |> fix_player_names(),
               # Match the trailing threshold, not digits that might occur in a name.
               line = as.numeric(str_match(outcome_name, "([0-9]+)\\+\\s*$")[, 2]) - 0.5)
    if (anyNA(data$line)) stop("BetRight: unsupported player threshold in ", labels[[stat]])
    data <- data |> mutate(player_key = str_to_lower(player_name)) |>
        left_join(roster |> transmute(player_key = str_to_lower(player_name),
                                     canonical_name = player_name, player_team),
                  by = "player_key", relationship = "many-to-one") |>
        mutate(player_name = coalesce(canonical_name, player_name))
    invalid <- is.na(data$player_team) |
        !(data$player_team == data$home_team | data$player_team == data$away_team)
    if (any(invalid)) {
        warning("BetRight roster names unresolved: ", paste(unique(data$player_name[invalid]), collapse = ", "), call. = FALSE)
        data$player_team[invalid] <- NA_character_
    }
    data |> transmute(match, home_team, away_team, market_name = labels[[stat]], player_name,
                      player_team, line, over_price = price, agency = "BetRight", event_id,
                      outcome_name, outcome_id, fixed_market_id,
                      opposition_team = case_when(player_team == home_team ~ away_team,
                                                  player_team == away_team ~ home_team,
                                                  TRUE ~ NA_character_))
}

main_betright <- function(fetch = betright_get_json, output_dir = data_paths$raw_odds,
                          roster = read_csv(data_file("raw_stats", "supercoach-data.csv"), show_col_types = FALSE)) {
    roster <- roster |> transmute(player_name = fix_player_names(paste(player_first_name, player_last_name)),
                                  player_team = fix_team_names(player_team)) |> distinct()
    if (anyDuplicated(str_to_lower(roster$player_name))) stop("BetRight: ambiguous roster names.")
    matches <- betright_matches(fetch("https://next-api.betright.com.au/Sports/Category?categoryId=110"))
    rows <- betright_empty_rows()
    for (id in matches$match_id) {
        response <- fetch(paste0("https://next-api.betright.com.au/Sports/MasterEventEvents?masterEventId=", id))
        rows <- bind_rows(rows, parse_betright_event(response, id))
    }
    rows <- rows |> left_join(matches, by = "match_id", relationship = "many-to-one")
    h2h <- rows |> filter(event_name == "Money Line") |> mutate(team = fix_team_names(outcome_name))
    home <- h2h |> filter(team == home_team) |>
        select(match_id, match, start_time, home_team, away_team, home_win = price)
    away <- h2h |> filter(team == away_team) |> select(match_id, away_win = price)
    head_to_head <- inner_join(home, away, by = "match_id", relationship = "one-to-one") |>
        transmute(match, start_time, market_name = "Head To Head", home_team, home_win, away_team,
                  away_win, margin = round(1 / home_win + 1 / away_win, 3), agency = "BetRight")
    outputs <- list(betright_h2h = head_to_head)
    for (stat in c("points", "rebounds", "assists", "pras", "threes")) {
        outputs[[paste0("betright_player_", stat)]] <- betright_player_market(rows, stat, roster)
    }
    # Complete every fetch and parse before replacing existing CSVs.
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    iwalk(outputs, ~ write_csv(.x, file.path(output_dir, paste0(.y, ".csv"))))
    message("BetRight: refreshed ", nrow(head_to_head), " matches and ",
            sum(map_int(outputs[-1], nrow)), " player-prop rows.")
    invisible(outputs)
}

if (!isTRUE(getOption("nbl.betright.skip_run", FALSE))) main_betright()

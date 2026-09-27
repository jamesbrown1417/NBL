# Sportsbet season rollover — 19 September 2026

Updated only the Sportsbet scraper. Existing shared configuration is 2026–2027.

## Verified changes

- Replaced obsolete generated CSS selectors with event and outcome automation attributes.
- Parsed each card together and corrected away-at-home fixture orientation, including odds.
- Reused one match snapshot for head-to-head and player props.
- Consulted event metadata before requesting optional market groups. PRA group 782 is not advertised for the four current fixtures; its CSV has headers and zero rows.
- Retained SGM external IDs, handled empty prop responses with typed columns, excluded inactive or unpriced selections, and exposed request errors.
- Removed repeated PRA and steals writes.
- Warned about roster conflicts and cleared team/opposition for unresolved players.

## Validation

14 offline assertions passed. A full live run completed and refreshed the Sportsbet CSVs. Checked populated prop prices and SGM event/selection IDs. No other bookmaker was run or edited.

| Output | Rows |
| --- | ---: |
| sportsbet_h2h.csv | 4 |
| sportsbet_player_assists.csv | 89 |
| sportsbet_player_blocks.csv | 47 |
| sportsbet_player_points.csv | 184 |
| sportsbet_player_pras.csv | 0 |
| sportsbet_player_rebounds.csv | 141 |
| sportsbet_player_steals.csv | 79 |
| sportsbet_player_threes.csv | 151 |

## Roster update resolved

The full-player API is now hosted at `www.supercoach.com.au`. The 2026 `players-cf` endpoint returned 148 players across all 10 teams with the existing schema. Updated `Scripts/03-scrape-supercoach.R` to use that domain, send JSON request headers, apply a timeout, and validate player records, unique IDs and team coverage before replacing the roster.

The user-provided `userteams/13618/statsPlayers` endpoint requires authentication; the full-player endpoint succeeds without account credentials. Refreshed `Data/raw/stats/supercoach-data.csv` and reran Sportsbet. All 17 previously flagged players now map to a team in their fixture, and every exported player prop has a valid opposing team. The 14 Sportsbet regression assertions also pass with the updated roster.

## Run

From the repository root: `Rscript OddsScraper/scrape_sportsbet.R`.

This refreshes Sportsbet raw odds only. It does not run downstream processing, publish reports, or commit changes. Request failures are surfaced; when run directly, existing output files remain if the scrape fails before writes. Their timestamps must not be treated as a successful refresh.

# PointsBet season rollover — 19 September 2026

Status: updated scraper completed a live refresh.

The competition 7172 endpoint and MES v3 event endpoints still work. They return four opening-round fixtures. Event details contain 23, 21, 20 and 22 markets respectively, without standard player points, rebounds, assists or threes markets. The website lists the same market counts; Melbourne–Adelaide's All Markets page contains match markets and a Cole World promotional market, which is not a standalone player-stat market and is excluded.

## Changes

- Discover event keys independently of featured markets, including events with no featured prices.
- Validate competition and event responses, apply a request timeout, and propagate errors.
- Refuse a paginated discovery response instead of silently publishing a partial competition.
- Parse typed empty responses and refresh unavailable player-market CSVs with headers and zero rows.
- Exclude closed, hidden, unpriced, live and explicitly partial-game selections.
- Match head-to-head prices by team identity using event details.
- Normalize the refreshed roster and flag team assignments outside the fixture.
- Preserve over/under SGM keys, pair by explicit event/market/player/line identity, and retain under-only rows.
- Finish all fetches and parsing before writing CSVs, so an API failure leaves existing outputs unchanged. Filesystem write failures are not an atomic transaction across all five files.

## Validation and output

27 regression assertions pass, covering head-to-head order, empty featured markets, alternative stat formats, paired prices and IDs, under-only markets, suspension/visibility, empty competition responses, roster conflicts, and preservation of previous output after request failure.

A live `Rscript OddsScraper/scrape_pointsbet.R` run refreshed four head-to-head rows. Every home/away price pair matched the captured API data. Points, rebounds, assists and threes CSVs now contain zero data rows with the full existing schema, replacing stale previous-season props. No standard player markets were available to validate against live selections; those parsing paths were tested with representative fixtures. SGM pricing calls were not tested.

Only PointsBet was changed in this step. Downstream processing and publication were not run.

# BetRight season rollover — 19 September 2026

Status: live refresh completed and checked against captured responses.

The existing category 110 and MasterEventEvents API endpoints still work. Four fixtures offer points, rebounds, assists, PRAs and threes. The current threes market is named “Player Three Pointers”, which the previous scraper did not recognise. Several optional outcome fields used by the previous parser are absent in the current response.

## Changes

- Locate NBL by category ID instead of relying on the first category; exclude futures.
- Discover fixtures independently of featured prices and retain IDs of any length as strings.
- Fetch each fixture once for all supported markets, reducing a four-match run from 21 requests to 5.
- Read head-to-head selections by team identity rather than response ordering.
- Parse typed rows without requiring absent outcome title/header fields.
- Recognise current threes names, restrict player patterns to full-game markets, and extract trailing integer-plus thresholds as half-point lines.
- Normalize teams before assigning opponents and resolve case-only player-name differences to canonical roster names.
- Preserve event, outcome and fixed-market IDs for the existing SGM consumer.
- Handle empty market responses with six header-only CSVs; fail visibly on malformed payloads, unsupported player thresholds, or request failures.
- Complete all requests and parsing before writing outputs. This protects existing CSVs from request/parse failures; the six filesystem writes are not one atomic transaction.

## Validation

40 regression assertions pass. The live refresh produced four head-to-head rows and the following player props:

| Market | Rows |
| --- | ---: |
| Points | 215 |
| Rebounds | 113 |
| Assists | 57 |
| Threes | 99 |
| Pras | 104 |

All exported player prices, thresholds and three-part SGM identifiers match the captured API responses. All players map to a team in their fixture with the correct opposition. All four head-to-head price pairs match the source.

This feed supplies alternate overs for the supported player markets. Paired unders and other market categories were not added. Live SGM pricing calls were not tested. Downstream processing/publication and other agency scrapers were not run.

Run from the project root: `Rscript OddsScraper/scrape_BetRight.R`.

# TAB season rollover — 19 September 2026

Status: current user-supplied NSW response validated and imported; unattended browser capture remains unverified.

## Findings

The existing response was saved on 1 April 2026 and contains Adelaide v Sydney with suspended markets. The current TAB website displays Melbourne v Adelaide, Perth v South East Melbourne, New Zealand v Illawarra, and Sydney v Cairns. Direct requests to the configured competition API failed (HTTP/2 stream error, then HTTP/1.1 timeout); the in-app browser also could not open that API URL. This does not establish whether the endpoint has moved or requires a different browser/session context. Requested the current competition request URL from the user.

## Changes

- Reject missing or older-than-30-minute response files before writing CSVs.
- Handle explicit empty match/market lists with the existing six output schemas.
- Exclude suspended, closed and unpriced propositions.
- Pair head-to-head prices by team name rather than response ordering.
- Include the fixture in player-line deduplication.
- Resolve full names, initials and unambiguous surnames within the fixture teams, preserving multiword surnames and unresolved labels.
- Retain over/under proposition IDs and support integer total lines.
- Propagate parser errors and finish parsing all outputs before writing.
- Validate browser JSON, decode HTML entities, replace the response atomically, and exit nonzero on capture errors; add a 90-second overall capture timeout.
- Allow a confirmed replacement endpoint through `TAB_NBL_API_URL` without changing SA jurisdiction by default.

## Validation

31 R assertions and three Python unit tests pass, covering fixtures, paired prices and IDs, repeated player lines across fixtures, compound and abbreviated surnames, ambiguous names, alternate points/assists/rebounds/threes, empty and suspended responses, stale files, malformed captures, and preservation of a saved response on write failure.

The user subsequently supplied the current competition JSON. It contains 97, 99, 95 and 96 markets across four fixtures, including all four supported player-prop categories. Its self link identifies NSW and does not include `numTopMarkets`; this confirms coverage of the supplied response, not the behaviour of that parameter in every request.

Added the observed “Parker J-Cartwright” alias and a `--input-json` capture option. Imported the supplied response and refreshed all six TAB output files:

| Output | Rows |
| --- | ---: |
| Head to head | 4 |
| Total points | 4 |
| Player points | 193 |
| Player rebounds | 137 |
| Player assists | 93 |
| Player threes | 105 |

All 528 player rows preserve their source proposition ID, price and threshold, and resolve to a roster team and opponent within the fixture. No unresolved players remain. Player props in this snapshot are alternate overs; under prices and IDs remain blank because the source does not offer paired unders for those markets.

## Capture limitation and jurisdiction

The data just imported is NSW. The existing unattended capture default remains SA, with `TAB_NBL_API_URL` available for an explicit override. No claim is made that SA and NSW prices are identical. The original API path is confirmed by the user, but automated requests from this environment still fail. Browser capture has not passed an end-to-end run here.

For a fresh browser response saved as JSON, run `python3 OddsScraper/TAB/get-TAB-response.py --input-json /absolute/path/response.json`, followed by `Rscript OddsScraper/TAB/scrape_TAB.R` from the project root. Import rejects files older than 30 minutes and preserves the source modification time so a copied file is not automatically treated as a new capture. Obtain a genuinely fresh API response before each refresh; a filesystem timestamp alone cannot prove the underlying odds are current.

Other bookmaker scripts were not changed, and downstream processing/publication was not run.

# Data layout

- `reference/` contains manually maintained inputs.
- `raw/stats/` contains current source extracts used by the stats pipeline.
- `raw/odds/` contains current bookmaker extracts and response captures.
- `processed/stats/` contains canonical and derived stats datasets.
- `processed/odds/` contains normalized current-market datasets consumed by the app and reports.
- `archive/odds/` contains dated odds snapshots.

Current raw and processed artifacts are intentionally tracked in Git. Season and path settings live in `Scripts/00-config.R`. Fixture ingestion is currently deferred and there is no schedule dataset contract.

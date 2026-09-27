#!/bin/bash

set -euo pipefail
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

PROJECT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_DIR"

mkdir -p \
  Data/raw/odds/responses/tab \
  Data/processed/odds \
  Data/processed/stats

/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/TAB/get-TAB-response.py
Rscript OddsScraper/master_processing_script.R
Rscript Scripts/12-export-web-stats.R
# Model prices for the web odds export; a failure leaves the export without them.
Rscript Models/props/03-price-current-markets.R || echo "Model pricing failed; exporting odds without model prices" >&2
Rscript Scripts/13-export-web-odds.R

git add -- Data/raw Data/processed Reports/odds_summary.html web/public/data

if ! git diff --cached --quiet -- Data/raw Data/processed Reports/odds_summary.html web/public/data; then
  git commit -m "automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')" -- \
    Data/raw Data/processed Reports/odds_summary.html web/public/data
  git push origin main
fi

echo "1" | quarto publish quarto-pub Reports/odds_summary.qmd

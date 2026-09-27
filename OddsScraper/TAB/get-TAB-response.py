import asyncio
import argparse
import json
import os
from html import unescape
import re
import tempfile
import time
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[2]
OUTPUT_PATH = PROJECT_ROOT / "Data" / "raw" / "odds" / "responses" / "tab" / "tab_response.json"

API_URL = os.environ.get(
    "TAB_NBL_API_URL",
    "https://api.beta.tab.com.au/v1/tab-info-service/sports/Basketball/competitions/NBL?jurisdiction=NSW&numTopMarkets=5",
)


def parse_response(page_content):
    """Accept browser-rendered JSON, but never mistake an error page for odds."""
    match = re.search(r"<pre[^>]*>(.*?)</pre>", page_content, re.DOTALL)
    content = unescape(match.group(1)) if match else page_content
    data = json.loads(content)
    if not isinstance(data, dict) or not isinstance(data.get("matches"), list):
        raise ValueError("Expected a TAB competition response containing a matches list")
    for event in data["matches"]:
        if not isinstance(event, dict) or not event.get("name") or not event.get("startTime") or not isinstance(event.get("markets"), list):
            raise ValueError("Incomplete TAB match record")
    return data


def save_response(data, output_path=OUTPUT_PATH):
    """Replace the last response only after a complete file has been written."""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    temp_path = None
    try:
        with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=output_path.parent,
                                         suffix=".tmp", delete=False) as stream:
            temp_path = Path(stream.name)
            json.dump(data, stream, indent=2)
        temp_path.replace(output_path)
    finally:
        if temp_path is not None:
            temp_path.unlink(missing_ok=True)


async def main():
    from selenium_driverless import webdriver
    options = webdriver.ChromeOptions()
    options.add_argument("--disable-blink-features=AutomationControlled")

    
    async with webdriver.Chrome(options=options) as driver:
        await driver.minimize_window()
        # First establish session on main site
        await driver.get("https://www.tab.com.au")
        await driver.sleep(3)
        
        # Now fetch the API directly through the browser
        api_url = API_URL
        await driver.get(api_url)
        await driver.sleep(2)
        
        # Get the JSON response
        page_content = await driver.page_source
        
        # Parse JSON from the page
        try:
            data = parse_response(page_content)
            save_response(data)
            print(f"[SUCCESS] Saved API data to {OUTPUT_PATH}")
            
            # Quick summary of what was saved
            if "matches" in data:
                print(f"[INFO] Saved {len(data.get('matches', []))} matches")
            
        except (json.JSONDecodeError, ValueError):
            print("[ERROR] Could not parse JSON from response")
            # Save debug file in same directory
            debug_path = OUTPUT_PATH.with_name("tab_response_debug.html")
            debug_path.parent.mkdir(parents=True, exist_ok=True)
            with debug_path.open("w", encoding="utf-8") as f:
                f.write(page_content)
            print(f"[DEBUG] Saved raw response to {debug_path}")
            raise

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Capture TAB odds or import a saved competition response.")
    parser.add_argument("--input-json", type=Path, help="Import a freshly saved TAB JSON response instead of opening Chrome")
    args = parser.parse_args()
    if args.input_json:
        # Preserve the source timestamp so importing an old capture cannot make it fresh.
        age = time.time() - args.input_json.stat().st_mtime
        if age > 1800:
            parser.error("Saved response is older than 30 minutes; obtain a fresh capture.")
        data = parse_response(args.input_json.read_text(encoding="utf-8"))
        save_response(data)
        source_time = args.input_json.stat().st_mtime
        os.utime(OUTPUT_PATH, (source_time, source_time))
        print(f"[SUCCESS] Imported {len(data['matches'])} matches into {OUTPUT_PATH}")
        print(f"[SOURCE] {data.get('_links', {}).get('self', 'Not supplied')}")
    else:
        asyncio.run(asyncio.wait_for(main(), timeout=90))

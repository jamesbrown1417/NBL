# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in CSV of URLs=========================================================
import pandas as pd
# Read csv (no header col)
url_df = pd.read_csv('OddsScraper/Bet365/player_urls.csv', header=None)

# Convert first column to a list
url_df = url_df[0]

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")
    
    async with webdriver.Chrome(options=options) as driver:
        for index, url in enumerate(url_df, start=1): # Start counting from 1 for match_n
            try:
                await driver.get(url)
                
                # Wait for cm-MarketGroupWithIconsButton_Text to exist
                await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ')]", timeout=5)
                
                # Print URL
                print(f"Getting URL {url} which is match {index}")
                
                # If there is a button that says Player Assists, click it
                try:
                    player_assists_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Player Assists']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_assists_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_assists_button.click()
                    print('Clicked Player Assists')
                    await driver.sleep(1)
                except:
                    print('No Player Assists button was found')
                    pass

                # If there is a button that says Player Assists Milestones, click it
                try:
                    assists_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Player Assists Milestones']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", assists_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await assists_milestones_button.click()
                    print('Clicked Player Assists Milestones')
                    await driver.sleep(1)
                except:
                    print('No Player Assists Milestones button was found')
                    pass

                # If there is a button that says Player Rebounds, click it
                try:
                    rebounds_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Player Rebounds']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", rebounds_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await rebounds_button.click()
                    print('Clicked Player Rebounds')
                    await driver.sleep(1)
                except:
                    print('No Player Rebounds button was found')
                    pass

                # If there is a button that says Player Rebounds Milestones, click it
                try:
                    rebounds_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Player Rebounds Milestones']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", rebounds_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await rebounds_milestones_button.click()
                    print('Clicked Player Rebounds Milestones')
                    await driver.sleep(1)
                except:
                    print('No Player Rebounds Milestones button was found')
                    pass

                # If there is a button that says Player Threes Made, click it
                try:
                    threes_made_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Player Threes Made']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", threes_made_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await threes_made_button.click()
                    print('Clicked Player Threes Made')
                    await driver.sleep(1)
                except:
                    print('No Player Threes Made button was found')
                    pass

                # If there is a button that says Player Threes Made Milestones, click it
                try:
                    threes_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Player Threes Made Milestones']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", threes_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await threes_milestones_button.click()
                    print('Clicked Player Threes Made Milestones')
                    await driver.sleep(1)
                except:
                    print('No Player Threes Made Milestones button was found')
                    pass

                # Get all elements with class 'msl-ShowMore_Link ' that has text 'Show more'
                button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]")
                
                print(len(button_elements))
                    
                # Scroll into view of each button, click it and wait 1 second
                for button_element in button_elements:
                   await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                   await driver.execute_script("window.scrollBy(0, -150)")
                   await button_element.click()
                   await driver.sleep(1)
                    
                # Write out html to file------------------------------------------------
                # wait for elem to exist
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_players = await elem.get_attribute('outerHTML')
                with open(f"OddsScraper/Bet365/HTML/body_html_players_match_{index}.txt", 'w') as f:
                    f.write(body_html_players)
                        
            except Exception as e:
                print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
                continue  # Proceed to the next iteration of the loop

asyncio.run(main())                    
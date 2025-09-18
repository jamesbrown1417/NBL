# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime
import os
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

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

async def login_to_bet365(driver):
    """Login to Bet365 using credentials from environment variables."""
    username = os.getenv('BET365USER')
    password = os.getenv('BET365PW')
    
    if not username or not password:
        raise ValueError("BET365USER and BET365PW environment variables must be set")
    
    print("Logging into Bet365...")
    
    # Go to main site
    await driver.get('https://www.bet365.com.au')
    await driver.sleep(3)
    
    try:
        # Look for login button/link
        login_element = await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')]", timeout=10)
        await login_element.click()
        await driver.sleep(1)
        
        # Enter username - try multiple methods
        try:
            username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username']", timeout=10)
        except:
            username_field = await driver.find_element(By.XPATH, "//input[contains(@class, 'lms-StandardLogin_NoControl')]", timeout=5)
        
        # Clear field and enter username using JavaScript
        await username_field.click()
        await driver.sleep(0.5)
        
        # Try JavaScript method first (more reliable)
        try:
            await driver.execute_script(f"arguments[0].value = '{username}';", username_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('input', { bubbles: true }));", username_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('change', { bubbles: true }));", username_field)
            print(f"Set username via JavaScript: {username}")
        except:
            # Fallback to send_keys all at once
            await username_field.clear()
            await driver.sleep(0.3)
            await username_field.send_keys(username)
            print(f"Set username via send_keys: {username}")
        
        await driver.sleep(1)
        print(f"Entered username: {username}")
        
        # Enter password - try multiple methods
        try:
            password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=5)
        except:
            password_field = await driver.find_element(By.XPATH, "//input[contains(@class, 'lms-StandardLogin_Password')]", timeout=5)
        
        # Clear field and enter password using JavaScript
        await password_field.click()
        await driver.sleep(0.5)
        
        # Try JavaScript method first (more reliable)
        try:
            await driver.execute_script(f"arguments[0].value = '{password}';", password_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('input', { bubbles: true }));", password_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('change', { bubbles: true }));", password_field)
            print("Set password via JavaScript")
        except:
            # Fallback to send_keys all at once
            await password_field.clear()
            await driver.sleep(0.3)
            await password_field.send_keys(password)
            print("Set password via send_keys")
        
        await driver.sleep(1)
        print("Entered password")
        
        # Click login button - try multiple selectors
        try:
            login_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'lms-StandardLogin_Login')]", timeout=5)
        except:
            try:
                login_button = await driver.find_element(By.XPATH, "//input[@type='submit']", timeout=5)
            except:
                login_button = await driver.find_element(By.XPATH, "//button[contains(text(), 'Login')]", timeout=5)
        
        await login_button.click()
        print("Clicked login button")
        await driver.sleep(5)
        
        # Wait for login to complete - look for user account indicator
        try:
            await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-Balance')]", timeout=15)
            print("Successfully logged in to Bet365")
            return True
        except:
            print("Login may have failed - no balance indicator found")
            return False
            
    except Exception as e:
        print(f"Login failed: {e}")
        return False

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")
    
    # Add user agent to avoid detection
    options.add_argument("--user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    
    async with webdriver.Chrome(options=options) as driver:
        # First login to Bet365
        login_success = await login_to_bet365(driver)
        if not login_success:
            print("Warning: Login failed, continuing without authentication")
        
        await driver.sleep(2)
        for index, url in enumerate(url_df, start=1): # Start counting from 1 for match_n
            try:
                await driver.get(url)
                
                # Wait for cm-MarketGroupWithIconsButton_Text to exist
                await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ')]", timeout=5)
                
                # Print URL
                print(f"Getting URL {url} which is match {index}")
                
                # If there is a button that says Player Assists, click it
                try:
                    player_assists_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Assists O/U']")
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
                    assists_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Assists']")
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
                    rebounds_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Rebounds O/U']")
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
                    rebounds_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Rebounds']")
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
                    threes_made_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Threes Made O/U']")
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
                    threes_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Threes Made']")
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

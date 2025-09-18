"""
Test script to verify Bet365 login functionality.
"""

# Import Modules
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
import asyncio
import os
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

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
        await driver.sleep(2)
        
        # Enter username - try multiple methods
        try:
            username_field = await driver.find_element(By.XPATH, "//input[contains(@class, 'lms-StandardLogin_Username')]", timeout=5)
        except:
            username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username']", timeout=10)
        
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
            password_field = await driver.find_element(By.XPATH, "//input[contains(@class, 'lms-StandardLogin_Password')]", timeout=5)
        except:
            password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=5)
        
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
            login_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'lms-LoginButton ')]", timeout=5)
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
            print("✅ Successfully logged in to Bet365")
            return True
        except:
            print("❌ Login may have failed - no balance indicator found")
            return False
            
    except Exception as e:
        print(f"❌ Login failed: {e}")
        return False

async def test_login():
    """Test the login functionality."""
    options = webdriver.ChromeOptions()
    # Remove headless mode for testing so we can see what's happening
    # options.add_argument("--headless=True")
    
    # Add user agent to avoid detection
    options.add_argument("--user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    
    async with webdriver.Chrome(options=options) as driver:
        login_success = await login_to_bet365(driver)
        
        if login_success:
            print("🎉 Login test passed!")
            # Stay on the page for a moment to verify
            await driver.sleep(5)
        else:
            print("💥 Login test failed!")
            
        return login_success

if __name__ == "__main__":
    print("Testing Bet365 login functionality...")
    result = asyncio.run(test_login())
    if result:
        print("✅ Test completed successfully")
    else:
        print("❌ Test failed")
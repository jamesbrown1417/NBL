"""
Advanced test script for Bet365 login with multiple fallback methods.
"""

from selenium_driverless import webdriver
from selenium_driverless.types.by import By
import asyncio
import os
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

async def login_to_bet365_advanced(driver):
    """Login to Bet365 with multiple fallback methods for text input."""
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
        await driver.sleep(3)
        
        # Enter username with multiple fallback methods
        username_field = None
        try:
            username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username']", timeout=10)
            print("Found username field by placeholder")
        except:
            try:
                username_field = await driver.find_element(By.XPATH, "//input[contains(@class, 'lms-StandardLogin_Username')]", timeout=5)
                print("Found username field by class")
            except:
                username_field = await driver.find_element(By.XPATH, "//input[@type='text']", timeout=5)
                print("Found username field by type")
        
        # Try multiple methods to enter username
        await username_field.click()
        await driver.sleep(0.5)
        
        # Method 1: JavaScript setValue
        try:
            await driver.execute_script(f"arguments[0].value = '{username}';", username_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('input', { bubbles: true }));", username_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('change', { bubbles: true }));", username_field)
            print(f"✅ Set username via JavaScript: {username}")
        except Exception as e:
            print(f"❌ JavaScript method failed: {e}")
            
            # Method 2: Clear and send_keys slowly
            try:
                await username_field.clear()
                await driver.sleep(0.5)
                for char in username:
                    await username_field.send_keys(char)
                    await driver.sleep(0.15)
                print(f"✅ Set username via send_keys: {username}")
            except Exception as e2:
                print(f"❌ send_keys method failed: {e2}")
        
        await driver.sleep(2)
        
        # Enter password with multiple fallback methods
        password_field = None
        try:
            password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=5)
            print("Found password field by placeholder")
        except:
            try:
                password_field = await driver.find_element(By.XPATH, "//input[contains(@class, 'lms-StandardLogin_Password')]", timeout=5)
                print("Found password field by class")
            except:
                password_field = await driver.find_element(By.XPATH, "//input[@type='password']", timeout=5)
                print("Found password field by type")
        
        # Try multiple methods to enter password
        await password_field.click()
        await driver.sleep(0.5)
        
        # Method 1: JavaScript setValue
        try:
            await driver.execute_script(f"arguments[0].value = '{password}';", password_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('input', { bubbles: true }));", password_field)
            await driver.execute_script("arguments[0].dispatchEvent(new Event('change', { bubbles: true }));", password_field)
            print("✅ Set password via JavaScript")
        except Exception as e:
            print(f"❌ JavaScript method failed: {e}")
            
            # Method 2: Clear and send_keys slowly
            try:
                await password_field.clear()
                await driver.sleep(0.5)
                for char in password:
                    await password_field.send_keys(char)
                    await driver.sleep(0.15)
                print("✅ Set password via send_keys")
            except Exception as e2:
                print(f"❌ send_keys method failed: {e2}")
        
        await driver.sleep(2)
        
        # Check if fields have values
        username_value = await username_field.get_attribute('value')
        password_value = await password_field.get_attribute('value')
        print(f"Username field value: '{username_value}'")
        print(f"Password field has value: {len(password_value) > 0}")
        
        # Click login button
        login_button = None
        try:
            login_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'lms-StandardLogin_Login')]", timeout=5)
            print("Found login button by class")
        except:
            try:
                login_button = await driver.find_element(By.XPATH, "//input[@type='submit']", timeout=5)
                print("Found login button by submit type")
            except:
                login_button = await driver.find_element(By.XPATH, "//button[contains(text(), 'Login')]", timeout=5)
                print("Found login button by text")
        
        await login_button.click()
        print("Clicked login button")
        await driver.sleep(5)
        
        # Wait for login to complete
        try:
            balance_element = await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-Balance')]", timeout=15)
            print("✅ Successfully logged in to Bet365 - found balance")
            return True
        except:
            # Try alternative success indicators
            try:
                await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedIn')]", timeout=5)
                print("✅ Successfully logged in to Bet365 - found logged in header")
                return True
            except:
                print("❌ Login may have failed - no success indicators found")
                
                # Check for error messages
                try:
                    error_msg = await driver.find_element(By.XPATH, "//*[contains(text(), 'incorrect') or contains(text(), 'Invalid')]", timeout=3)
                    error_text = await error_msg.get_attribute('innerText')
                    print(f"❌ Login error: {error_text}")
                except:
                    print("❌ No specific error message found")
                
                return False
            
    except Exception as e:
        print(f"❌ Login failed: {e}")
        return False

async def test_advanced_login():
    """Test the advanced login functionality."""
    options = webdriver.ChromeOptions()
    # Remove headless mode for debugging
    # options.add_argument("--headless=True")
    
    # Add more options to avoid detection
    options.add_argument("--user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    options.add_argument("--disable-blink-features=AutomationControlled")
    options.add_experimental_option("excludeSwitches", ["enable-automation"])
    options.add_experimental_option('useAutomationExtension', False)
    
    async with webdriver.Chrome(options=options) as driver:
        # Execute script to remove webdriver property
        await driver.execute_script("Object.defineProperty(navigator, 'webdriver', {get: () => undefined})")
        
        login_success = await login_to_bet365_advanced(driver)
        
        if login_success:
            print("🎉 Advanced login test passed!")
            await driver.sleep(10)  # Stay longer to verify
        else:
            print("💥 Advanced login test failed!")
            
        return login_success

if __name__ == "__main__":
    print("Testing advanced Bet365 login functionality...")
    result = asyncio.run(test_advanced_login())
    if result:
        print("✅ Advanced test completed successfully")
    else:
        print("❌ Advanced test failed")
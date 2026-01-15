#!/usr/bin/env python3
# tesla_charger_add_scrap.py

### Geospatial Analytics Lab
### Electric Vehicle Reach 
### By Edgar Daniel


### ----------------------------------------------------------------------------
### Libraries and Parameters ----------------------------------------------------


# Needed libraries 

# Web Scrapping 
import random
import time
import undetected_chromedriver as uc

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# System requiremenmts 
import datetime as dt
import argparse
import requests
import os


# Data Management 
import pandas as pd


# Parameters 

parser = argparse.ArgumentParser(
    description="Scraping code to get location data for both cargers and superchargers from official Tesla site. "
)

parser.add_argument(
    "--input_file",
    type=str,
    required=True,
    help="Output filename with coordinates for each charger/supercharger"
)

parser.add_argument(
    "--output_file",
    type=str,
    required=True,
    help="Output filename with coordinates for each charger/supercharger"
)

input_file = args.input_file
output_file = args.output_file



### ----------------------------------------------------------------------------
### Web Scrapping  -------------------------------------------------------------

## Chargers 

# Auxiliar function to mimic human waiting times 
def human_sleep(a=0.8, b=2.3):
    time.sleep(random.uniform(a, b))


# Anti bot detection configurration 
options = uc.ChromeOptions()
options.add_argument("--start-maximized")
options.add_argument("--disable-blink-features=AutomationControlled")
options.add_argument("--no-sandbox")
options.add_argument("--disable-dev-shm-usage")
options.binary_location = "/usr/bin/google-chrome"


# Get df data 
df = pd.read_csv(input_file)

# List of references 
hrefs = df.charger_href.drop_duplicates().to_list()

# Results list init 
results = []

# Link open loops 
for url in hrefs:

    # Call the driver 
    driver = uc.Chrome(options=options)
    wait = WebDriverWait(driver, 25)

    try:
        

        driver.get(url)

        original_window = driver.current_window_handle


        human_sleep(3, 4)

        button = driver.find_element(By.CSS_SELECTOR, "button[class*='tds-btn']")

        button.click()

        WebDriverWait(driver, 10).until(
            lambda d: len(d.window_handles) > 1
        )

        # Switch to the new window
        for handle in driver.window_handles:
            if handle != original_window:
                driver.switch_to.window(handle)
                break

        # Get the URL
        new_window_url = driver.current_url

        # Get the text of interes 
        new_window_url = new_window_url[new_window_url.index("daddr=")+6:]

        # Transorm text into float 
        coords = [float(x) for x in new_window_url.split(",")]
        lat, lon = coords[0],coords[1]

        print(f"Coordinate process completed for {url}. ")

    except Exception as e:
        lat, lon = 0.0,0.0

    # Append extraction results if any 
    results.append({
            "charger_href": url,
            "lat": lat,
            "lon": lon ,
        })

    print(f"Coordinate process failed for {url}. ")

    driver.quit()

# Results DataFrame
df_coords = pd.DataFrame()

# Final DataFrame
df = df.merge(
    df_coords, on = "charger_href", how = "left"
)

# Save into file 
df.to_csv(output_file, index = False)
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

args = parser.parse_args()

input_file = args.input_file
output_file = args.output_file


# OSM Mail 
EMAIL = os.getenv("OSM_EMAIL")
if not EMAIL:
    raise RuntimeError("OSM_EMAIL not set")


### ----------------------------------------------------------------------------
### Auxiliar Functions  --------------------------------------------------------


# Auxiliar function to mimic human waiting times 
def human_sleep(a=0.8, b=2.3):
    time.sleep(random.uniform(a, b))


# Auxiliar function to get coordinates 
def get_coordinates_nominatim(address: str, email: str):
    """
    Given a text address, return (lat, lon) using Nominatim (OpenStreetMap)
    """
    url = "https://nominatim.openstreetmap.org/search"
    params = {
        "q": address,
        "format": "json",
        "limit": 1
    }

    headers = {
        "User-Agent": f"your-app-name/1.0 ({email})"
    }

    response = requests.get(url, params=params, headers=headers, timeout=10)
    response.raise_for_status()

    data = response.json()

    if not data:
        return None, None

    return float(data[0]["lat"]), float(data[0]["lon"])



### ----------------------------------------------------------------------------
### Web Scrapping  -------------------------------------------------------------


# Get df data 
df = pd.read_csv(input_file)


df[["lat", "lon"]] = df["description"].apply(
            lambda x: pd.Series(
                get_coordinates_nominatim(x, EMAIL)
                )
        )


# Separete the ones with coords and without 
df_pend = df[df.lon.isna()]
df_nopen= df.dropna(subset=['lon'])


# List of references 
hrefs = df_pend.charger_href.to_list()
desc = df_pend.charger_href.to_list()

# Results list init 
results = []

failed_count = 0

# Link open loops 
for url, d in zip(hrefs, desc):

    # Anti bot detection configurration 
    options = uc.ChromeOptions()
    options.add_argument("--start-maximized")
    options.add_argument("--disable-blink-features=AutomationControlled")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    options.binary_location = "/usr/bin/google-chrome"

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
        lat, lon = coords[0], coords[1]
        failed_count = 0

        print(f"Coordinate process completed for {url}. ")

    except Exception as e:
        lat, lon = 0.0, 0.0
        failed_count +=1
        print(f"Coordinate process failed for {url}. ")

    if failed_count > 10:
        break

    # Append extraction results if any 
    results.append({
            "charger_href": url,
            "lat": lat,
            "lon": lon ,
        })

    driver.quit()

# Results DataFrame
df_coords = pd.DataFrame(results)

# Final DataFrame
df_pend = (
    df_pend
    .drop(columns=['lat', 'lon'])
    .merge(
    df_coords, on = "charger_href", how = "left"
    )
    .dropna()
    )

df = pd.concat([df_nopen, df_pend])

# Save into file 
df.to_csv(output_file, index = False)

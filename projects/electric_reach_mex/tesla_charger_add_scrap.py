#!/usr/bin/env python3
# tesla_charger_add_scrap.py

### Geospatial Analytics Lab
### Electric Vehicle Reach 
### By Edgar Daniel


### ----------------------------------------------------------------------------
### Libraries and Parameters ----------------------------------------------------


# Needed libraries 

# Web Scrapping 
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.actions.action_builder import ActionBuilder
from selenium.webdriver.common.actions.key_input import KeyInput
from selenium.webdriver.common.actions.pointer_input import PointerInput
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
import undetected_chromedriver as uc
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait


# System requiremenmts 
import time
import random
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
    "--output_file",
    type=str,
    required=True,
    help="Output filename with coordinates for each charger/supercharger"
)


group = parser.add_mutually_exclusive_group(required=True)

group.add_argument(
    "--links",
    nargs=2,
    metavar=("URL1", "URL2"),
    help="Two URLs to scrape (Charger link, Supercharger link)"
)

group.add_argument(
    "--country",
    help="Country name (e.g. Germany)"
)

args = parser.parse_args()

if args.links:
    URL_CHARGERS, URL_SUPERCHARGERS = args.links
    country = URL_CHARGERS[URL_CHARGERS.rindex("/"):]
    print("Links:", URL_CHARGERS, URL_SUPERCHARGERS)

if args.country:
    URL_CHARGERS, URL_SUPERCHARGERS =f"https://www.tesla.com/findus/list/chargers/{args.country}", f"https://www.tesla.com/findus/list/superchargers/{args.country}"
    country = args.country
    print("Country:", args.country)
    print("Links (Infered) :", URL_CHARGERS, URL_SUPERCHARGERS)

output_file = args.output_file


### ----------------------------------------------------------------------------
### Web Scrapping  -------------------------------------------------------------

## Chargers 
options = uc.ChromeOptions()
options.add_argument("--start-maximized")

# Invoke driver object 

driver = webdriver.Chrome(options=options)
wait = WebDriverWait(driver, 15)
actions = ActionChains(driver)
keyboard = KeyInput("keyboard")
actions_b = ActionBuilder(driver, keyboard=keyboard)



# Open Main page
driver = uc.Chrome(options=options)
wait = WebDriverWait(driver, 20)

driver.get(URL_CHARGERS)
time.sleep(3)

# Look for data items 
items =  driver.find_elements(
    By.CSS_SELECTOR,
    "div[class*='subregions_page_locations_container'] > div"
)
time.sleep(random.uniform(7,10))



results = []

for container in items:
    try:
        container_data = container.find_elements(
            By.CSS_SELECTOR, "div[class*='subregion_location_data']"
        )
    except Exception:
        continue

    for location in container_data:
        try:
            location_label = location.find_element(
                By.CSS_SELECTOR, "div[class*='subregion_location_title']"
            ).text.strip()
        except Exception:
            location_label = ""

        try:
            description_elements = location.find_elements(
                By.CSS_SELECTOR, "div[class*='subregion_location_addressLine1']"
            )
            address = ", ".join(
                el.text.strip() for el in description_elements if el.text.strip()
            )
        except Exception:
            address = ""

        results.append({
            "location_label": location_label,
            "description": address,
        })


driver.quit()
df_chargers = pd.DataFrame(results)




## Super - Chargers 

## Chargers 
options = uc.ChromeOptions()
options.add_argument("--start-maximized")

# Invoke driver object 

driver = webdriver.Chrome(options=options)
wait = WebDriverWait(driver, 15)
actions = ActionChains(driver)
keyboard = KeyInput("keyboard")
actions_b = ActionBuilder(driver, keyboard=keyboard)



# Open Main page
driver = uc.Chrome(options=options)
wait = WebDriverWait(driver, 20)

driver.get(URL_SUPERCHARGERS)
time.sleep(3)

# Look for data items 
items =  driver.find_elements(
    By.CSS_SELECTOR,
    "div[class*='subregions_page_locations_container'] > div"
)
time.sleep(random.uniform(7,10))



results = []

for container in items:
    try:
        container_data = container.find_elements(
            By.CSS_SELECTOR, "div[class*='subregion_location_data']"
        )
    except Exception:
        continue

    for location in container_data:
        try:
            location_label = location.find_element(
                By.CSS_SELECTOR, "div[class*='subregion_location_title']"
            ).text.strip()
        except Exception:
            location_label = ""

        try:
            description_elements = location.find_elements(
                By.CSS_SELECTOR, "div[class*='subregion_location_addressLine1']"
            )
            address = ", ".join(
                el.text.strip() for el in description_elements if el.text.strip()
            )
        except Exception:
            address = ""

        results.append({
            "location_label": location_label,
            "description": address,
        })


driver.quit()
df_superchargers = pd.DataFrame(results)


# Unify both DataFrames

df_chargers["charger_type"] = "charger"
df_superchargers["charger_type"] = "super_charger"

df = pd.concat([df_chargers, df_superchargers])

del df_chargers
del df_superchargers

df.to_csv(output_file, index = False)



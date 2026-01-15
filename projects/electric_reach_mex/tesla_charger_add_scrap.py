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


def human_sleep(a=0.8, b=2.3):
    time.sleep(random.uniform(a, b))


options = uc.ChromeOptions()
options.add_argument("--start-maximized")
options.add_argument("--disable-blink-features=AutomationControlled")
options.add_argument("--no-sandbox")
options.add_argument("--disable-dev-shm-usage")
options.binary_location = "/usr/bin/google-chrome"


driver = uc.Chrome(options=options)
wait = WebDriverWait(driver, 25)

driver.get(URL_CHARGERS)

# Wait for main container to load
wait.until(
    EC.presence_of_element_located(
        (By.CSS_SELECTOR, "div[class*='subregions_page_locations_container']")
    )
)

human_sleep(2, 4)

items = driver.find_elements(
    By.CSS_SELECTOR,
    "div[class*='subregions_page_locations_container'] > div"
)

results = []

for container in items:
    try:
        locations = container.find_elements(
            By.CSS_SELECTOR, "div[class*='subregion_location_data']"
        )
    except Exception:
        continue

    for location in locations:
        human_sleep(0.4, 1.1)

        # Location name
        try:
            location_label = location.find_element(
                By.CSS_SELECTOR, "div[class*='subregion_location_title']"
            ).text.strip()
            print("Location label extraction succeed.", end= "\r")
        except Exception:
            print("Location label extraction failed.", end= "\r")
            location_label = ""

        # Charger link (FIXED)
        try:
            link_el = location.find_element(
                By.CSS_SELECTOR, "a[href]"
            )
            charger_href = link_el.get_attribute("href")
            print("Address extraction succeed.", end= "\r")
        except Exception:
            print("Link extraction failed.", end= "\r")
            charger_href = ""

        # Address
        try:
            address_elements = location.find_elements(
                By.CSS_SELECTOR,
                "div[class*='subregion_location_addressLine1']"
            )
            address = ", ".join(
                el.text.strip() for el in address_elements if el.text.strip()
            )
            print("Address extraction succeed.", end= "\r")
        except Exception:
            print("Address extraction failed.", end= "\r")
            address = ""

        results.append({
            "location_label": location_label,
            "description": address,
            "charger_href": charger_href,
        })

human_sleep(3, 5)
driver.quit()

df_chargers = pd.DataFrame(results)




## Super - Chargers 

options = uc.ChromeOptions()
options.add_argument("--start-maximized")
options.add_argument("--disable-blink-features=AutomationControlled")
options.add_argument("--no-sandbox")
options.add_argument("--disable-dev-shm-usage")

driver = uc.Chrome(options=options)
wait = WebDriverWait(driver, 25)

driver.get(URL_CHARGERS)

# Wait for main container to load
wait.until(
    EC.presence_of_element_located(
        (By.CSS_SELECTOR, "div[class*='subregions_page_locations_container']")
    )
)

human_sleep(2, 4)

items = driver.find_elements(
    By.CSS_SELECTOR,
    "div[class*='subregions_page_locations_container'] > div"
)

results = []

for container in items:
    try:
        locations = container.find_elements(
            By.CSS_SELECTOR, "div[class*='subregion_location_data']"
        )
        print("Location extraction succeed.", end= "\r")
    except Exception:
        print("Location extraction failed.", end= "\r")
        continue

    for location in locations:
        human_sleep(0.4, 1.1)

        # Location name
        try:
            location_label = location.find_element(
                By.CSS_SELECTOR, "div[class*='subregion_location_title']"
            ).text.strip()
            print("Location label extraction succeed.", end= "\r")
        except Exception:
            print("Location label extraction failed.", end= "\r")
            location_label = ""

        # Charger link (FIXED)
        try:
            link_el = location.find_element(
                By.CSS_SELECTOR, "a[href]"
            )
            print("Link extraction succeed.", end= "\r")
            charger_href = link_el.get_attribute("href")
        except Exception:
            print("Link extraction failed.", end= "\r")
            charger_href = ""

        # Address
        try:
            address_elements = location.find_elements(
                By.CSS_SELECTOR,
                "div[class*='subregion_location_addressLine1']"
            )
            address = ", ".join(
                el.text.strip() for el in address_elements if el.text.strip()
            )

            print("Address extraction succeed.", end= "\r")
        except Exception:
            print("Address extraction failed.", end= "\r")
            address = ""

        results.append({
            "location_label": location_label,
            "description": address,
            "charger_href": charger_href,
        })

human_sleep(3, 5)
driver.quit()
df_superchargers = pd.DataFrame(results)


# Unify both DataFrames

df_chargers["charger_type"] = "charger"
df_superchargers["charger_type"] = "super_charger"

df = pd.concat([df_chargers, df_superchargers])

del df_chargers
del df_superchargers

df.to_csv(output_file, index = False)



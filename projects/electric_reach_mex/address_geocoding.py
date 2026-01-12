#!/usr/bin/env python3
# Address Geocoding 

### Geospatial Analytics Lab
### Electric Vehicle Reach 
### By Edgar Daniel


### ----------------------------------------------------------------------------
### Libraries and Parameters ----------------------------------------------------



# Needed libraries 

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
    "--input_file",
    type=str,
    required=True,
    help="Input file with address for each charger/supercharger"
)

parser.add_argument(
    "--out_file",
    type=str,
    required=True,
    help="Output filename with coordinates for each charger/supercharger"
)

args = parser.parse_args()


# Google Maps API
API_KEY = os.getenv("GOOGLE_MAPS_API_KEY")
if not API_KEY:
    raise RuntimeError("GOOGLE_MAPS_API_KEY not set")

# OSM Mail 
EMAIL = os.getenv("OSM_EMAIL")
if not EMAIL:
    raise RuntimeError("OSM_EMAIL not set")



### ----------------------------------------------------------------------------
### Auxiliar Functions ---------------------------------------------------------

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
        "User-Agent": f"tesla-charger/1.0 ({email})"
    }

    response = requests.get(url, params=params, headers=headers, timeout=10)
    response.raise_for_status()

    data = response.json()

    if not data:
        return None, None

    return float(data[0]["lat"]), float(data[0]["lon"])



def get_coordinates(address: str, api_key: str):
    
    """
    Given a text address, return (latitude, longitude)
    using Google Maps Geocoding API
    """

    url = "https://maps.googleapis.com/maps/api/geocode/json"
    params = {
        "address": address,
        "key": api_key
    }

    response = requests.get(url, params=params, timeout=10)
    data = response.json()

    if data["status"] != "OK":
        return None, None

    location = data["results"][0]["geometry"]["location"]
    return location["lat"], location["lng"]



### ----------------------------------------------------------------------------
### Address Geocoding  ---------------------------------------------------------


# Call the API 
df[["lat", "lon"]] = df["description"].apply(
    lambda x: pd.Series(get_coordinates(x, API_KEY))
)



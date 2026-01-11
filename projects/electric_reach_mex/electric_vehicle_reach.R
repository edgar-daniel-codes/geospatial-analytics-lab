

### ----------------------------------------------------------------------------
### Libraies and Parameters ----------------------------------------------------

# Install the needed libraries 
library(knitr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(readr)
library(maps)
library(leaflet)
library(leaflet.extras)

# Paths 
SAMPLE_DATA <- "../data/sample"

MARCOGEO_FOLDER <- "/marco_geo_2020" 
CENSO_FOLDER <- "/mex_censo_2020" 

CENSO_GEOJSON <-  "/inegi_censo_2020_urbageb.geojson"
CELLS_GEOJSON <-  "/cells_censo_2020_urbageb.geojson"

# Parameters
H3_RESOLUTION <- 8


### ----------------------------------------------------------------------------
### Data Preparation -----------------------------------------------------------




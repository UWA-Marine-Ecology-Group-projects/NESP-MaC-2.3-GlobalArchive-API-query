###
# Project: NESP MAC 2.3 GlobalArchive API Query
# Data:    Ningaloo Fish + Habitat Syntheses
# Task:    Use GlobalArchive API to query data, plot data and save as .csv
# Author:  Brooke Gibbons & Claude Spencer
# Date:    July 2023
##

# Load libraries needed -----
library(httr)
library(tidyverse)
library(RJSONIO)
library(rgdal)
library(sf)

# Bring in functions for API call ----
# TODO turn this into a package.
source("r/00_Functions-for-API (do not run).R")

synthesis_id <- 15 # For Ningaloo Fish synthesis

# API call for Metadata ----
metadata <- ga.api.metadata(synthesis_id)

# API call for Habitat ----
habitat <- ga.api.habitat(synthesis_id)

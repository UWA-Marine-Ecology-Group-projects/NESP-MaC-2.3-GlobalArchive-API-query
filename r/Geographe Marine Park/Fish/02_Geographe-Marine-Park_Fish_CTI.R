###
# Project: NESP MAC 2.3 GlobalArchive API Query
# Data:    Geographe Fish + Habitat Syntheses
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

synthesis_id <- 14 # For Geographe Fish + Habitat Syntheses

# API call for Metadata ----
metadata <- ga.api.metadata(synthesis_id)

# API call for Length ----
length <- ga.api.length(synthesis_id)

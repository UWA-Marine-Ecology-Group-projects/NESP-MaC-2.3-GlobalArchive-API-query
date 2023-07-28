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
library(cowplot)

# Bring in functions for API call ----
# TODO turn this into a package.
source("r/00_Functions-for-API (do not run).R")

# Set synthesis
synthesis <- "Ningaloo"
synthesis_id <- 15 # For Ningaloo Fish + Habitat Syntheses

# API call for Metadata ----
metadata <- ga.api.metadata(synthesis_id) %>%
  dplyr::filter(successful_length = TRUE)

# API call for Length ----
length <- ga.api.length(synthesis_id) %>%
  glimpse()

# Create "complete" length (a row for each species in every sample) ----
complete_length <- length %>%
  full_join(metadata) %>%
  dplyr::select(sample, caab, family, scientific, length_at_maturity, indicator, length, rms, precision, range, number) %>%
  tidyr::complete(sample, nesting(caab, family, scientific, length_at_maturity, indicator)) %>%
  dplyr::left_join(metadata, .) %>%
  dplyr::filter(successful_length = TRUE) %>%
  dplyr::filter(!is.na(scientific)) %>%
  tidyr::replace_na(list(number = 0))

indicators <- complete_length %>%
  dplyr::filter(indicator %in% "Yes")

length_bins_5 <- ga.five.bins.maturity(indicators)
length_bins_2 <- ga.two.bins.maturity(indicators)

# Save abundance by size class data as a csv ----
# Saved as two separate csvs as they include some of the same data
write.csv(length_bins_5, file = paste0("data/tidy/", synthesis, "_five-bins-size-of-maturity.csv"), row.names = F)
write.csv(length_bins_2, file = paste0("data/tidy/", synthesis, "_two-bins-size-of-maturity.csv"), row.names = F)

plot_size_of_maturity(length_bins_2, length_bins_5)

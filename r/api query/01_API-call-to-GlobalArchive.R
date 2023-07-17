###
# Project: API query
# Data:    Ningaloo & Geographe Habitat & Fish Syntheses
# Task:    Use GlobalArchive API to query data and save as RDS
# Author:  Brooke Gibbons
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
source("r/api query/00_Functions-for-API (do not run).R")

# Add Synthesis ID from web version ----
syntheses <- data.frame(name = c("Geographe-bay", "Ningaloo"),
                        id   = c(14,  15))

# Loop through syntheses----
for(row.num in 1:nrow(syntheses)){

  # Get information ----
  name <- syntheses$name[row.num]
  synthesis_id <- syntheses$id[row.num]

  # API call for Metadata ----
  metadata_raw <- ga.api.metadata(synthesis_id)

  # API call for Count ----
  count <- ga.api.count(synthesis_id)

  # API call for Length ----
  length <- ga.api.length(synthesis_id)

  # Create abundance by size class ----
  length_class <- length %>%
    dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm*10) %>%
    dplyr::select(sample, length, number, range, rms, precision, subject_common_name, family, genus, species, fb_length_at_maturity_mm) %>%
    dplyr::mutate(class = case_when(length > fb_length_at_maturity_mm ~ "100% +", length < fb_length_at_maturity_mm ~ "< 100%")) %>%
    dplyr::group_by(sample, family, genus, species, class) %>%
    dplyr::summarise(count = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(metadata) %>%
    tidyr::complete(sample, nesting(family, genus, species), class) %>%
    tidyr::replace_na(list(count = 0)) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::filter(scientific %in% indicator_species) %>%
    dplyr::filter(!is.na(class)) %>%
    dplyr::select(sample, family, genus, species, class, count, scientific) %>%
    dplyr::left_join(., metadata, by = "sample")

  length_sum <- length_class %>%
    dplyr::group_by(sample, class) %>%
    dplyr::summarise(count = sum(count)) %>%
    ungroup() %>%
    dplyr::mutate(scientific = "All indicator species") %>%
    dplyr::left_join(., metadata, by = "sample")

  length_combined <- bind_rows(length_sum, length_class)

  # Save raw data (if needed) ----
  saveRDS(metadata, paste0("output/", name, "_metadata.RDS"))
  saveRDS(count, paste0("output/", name, "_count.RDS"))
  saveRDS(length, paste0("output/", name, "_length.RDS"))
  saveRDS(length_combined, paste0("output/", name, "_length-class.RDS"))
}

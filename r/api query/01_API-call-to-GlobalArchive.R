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
# Run this version to update the shiny ----
syntheses <- data.frame(name = c("Geographe-bay", "Ningaloo"),
                        id   = c(14,  15))

# Run this version for a quick demo ----
syntheses <- data.frame(name = c("Geographe-bay"), #, "Ningaloo"
                        id   = c(14))              #,  15


# API call for Species Information ----
species_list <- ga.api.species.list(count)

# Loop through syntheses----
for(row.num in 1:nrow(syntheses)){

  # Get information ----
  name <- syntheses$name[row.num]
  synthesis_id <- syntheses$id[row.num]

  # API call for Metadata ----
  metadata_raw <- ga.api.metadata(synthesis_id) %>%
    dplyr::mutate(sample = url) %>%
    dplyr::mutate(coordinates = str_replace_all(.$coordinates, c("SRID=4326;POINT " = "", "[()]" = ""))) %>%
    tidyr::separate(coordinates, into = c("longitude", "latitude"), sep = " ") %>%
    dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
    dplyr::select(-c(status))

  # Add marine parks to metadata ----
  metadata_spatial <- metadata_raw

  # Add spatial
  coordinates(metadata_spatial) <- c('longitude', 'latitude')
  proj4string(metadata_spatial) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Add in marine spatial zoning information ----
  metadata <- bind_cols(metadata_raw, over(metadata_spatial, marineparks)) %>%
    dplyr::rename(zone = ZONE_TYPE) %>%
    tidyr::replace_na(list(status = "Fished"))

  metadata$zone <- fct_relevel(metadata$zone,
                               "National Park Zone",
                               "Sanctuary Zone",
                               "General Use",
                               "General Use Zone",
                               "Habitat Protection Zone",
                               "Multiple Use Zone",
                               "Recreational Use Zone",
                               "Special Purpose Zone (Mining Exclusion)")

  # API call for Count ----
  count <- ga.api.count(synthesis_id)

  # API call for Length ----
  length <- ga.api.length(synthesis_id)


  # # Add species information to count and length files ----
  # count_with_species <- count %>%
  #   mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
  #   left_join(., species_list, by = "subject")
  #
  # length_with_species <- length %>%
  #   mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
  #   left_join(., species_list, by = "subject")
  #
  # # Create abundance by size class ----
  # length_class <- length_with_species %>%
  #   dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm*10) %>%
  #   dplyr::select(sample, length, number, range, rms, precision, subject_common_name, family, genus, species, fb_length_at_maturity_mm) %>%
  #   dplyr::mutate(class = case_when(length > fb_length_at_maturity_mm ~ "100% +", length < fb_length_at_maturity_mm ~ "< 100%")) %>%
  #   dplyr::group_by(sample, family, genus, species, class) %>%
  #   dplyr::summarise(count = sum(number)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::full_join(metadata) %>%
  #   tidyr::complete(sample, nesting(family, genus, species), class) %>%
  #   tidyr::replace_na(list(count = 0)) %>%
  #   dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  #   dplyr::filter(scientific %in% indicator_species) %>%
  #   dplyr::filter(!is.na(class)) %>%
  #   dplyr::select(sample, family, genus, species, class, count, scientific) %>%
  #   dplyr::left_join(., metadata, by = "sample")
  #
  # length_sum <- length_class %>%
  #   dplyr::group_by(sample, class) %>%
  #   dplyr::summarise(count = sum(count)) %>%
  #   ungroup() %>%
  #   dplyr::mutate(scientific = "All indicator species") %>%
  #   dplyr::left_join(., metadata, by = "sample")
  #
  # length_combined <- bind_rows(length_sum, length_class)

  # Save raw data (if needed) ----
  saveRDS(metadata, paste0("output/", name, "_metadata.RDS"))
  # saveRDS(count_with_species, paste0("output/", name, "_count.RDS"))
  # saveRDS(length_with_species, paste0("output/", name, "_length.RDS"))
  # saveRDS(length_combined, paste0("output/", name, "_length-class.RDS"))

}

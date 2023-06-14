# Load libraries needed -----
library(httr)
library(tidyverse)
library(RJSONIO)

# Bring in functions for API call ----
# TODO turn this into a package.
source("r/api query/00_Functions-for-API (do not run).R")

# Define the Indicator species ----
indicator_species <- c("Chrysophrys auratus",
                       "Choerodon rubescens",
                       "Glaucosoma hebraicum",
                       "Epinephelides armatus",
                       "Centroberyx gerrardi",
                       "Nemadactylus valenciennesi",
                       "Achoerodus gouldii",
                       "Polyprion oxygeneios",
                       "Hyporthodus octofasciatus",
                       "Hyperoglyphe antarctica",
                       "Lethrinus punctulatus",
                       "Epinephelus multinotatus",
                       "Pristipomoides multidens",
                       "Pristipomoides spp",
                       "Lethrinus miniatus",
                       "Lethrinus nebulosus")

# Add Synthesis ID from web version ----
syntheses <- data.frame(name = c("Geographe-bay", "Ningaloo"),
                        id   = c(14,              15))

# Loop through syntheses----
for(row.num in 1:nrow(syntheses)){

  # Get information ----
  name <- syntheses$name[row.num]
  synthesis_id <- syntheses$id[row.num]

  # API call for Metadata ----
  metadata <- ga.api.metadata(synthesis_id) %>%
    dplyr::mutate(sample = url) %>%
    dplyr::mutate(coordinates = str_replace_all(.$coordinates, c("SRID=4326;POINT " = "", "[()]" = ""))) %>%
    tidyr::separate(coordinates, into = c("longitude", "latitude"), sep = " ") %>%
    dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))

  # API call for Count ----
  count <- ga.api.count(synthesis_id)

  # API call for Length ----
  length <- ga.api.length(synthesis_id)

  # API call for Species Information ----
  species_list <- ga.api.species.list(count)

  # Add species information to count and length files ----
  count_with_species <- count %>%
    mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
    left_join(., species_list)

  length_with_species <- length %>%
    mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
    left_join(., species_list)

  # Create abundance by size class ----
  length_class <- length_with_species %>%
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
    dplyr::left_join(., metadata)

  # Save raw data (if needed) ----
  saveRDS(metadata, paste0("output/", name, "_metadata.RDS"))
  saveRDS(count_with_species, paste0("output/", name, "_count.RDS"))
  saveRDS(length_with_species, paste0("output/", name, "_length.RDS"))
  saveRDS(length_class, paste0("output/", name, "_length-class.RDS"))

}

# PLOTS ----

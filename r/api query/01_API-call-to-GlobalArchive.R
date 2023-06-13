# Load libraries needed -----
library(httr)
library(tidyverse)
library(RJSONIO)

# Bring in functions for API call ----
# TODO turn this into a package.
source("r/api query/00_Functions-for-API (do not run).R")

# Add Synthesis ID from web version ----
# TODO loop through multiple syntheses.
synthesis_id = 14

metadata <- ga.api.metadata(synthesis_id) %>%
  dplyr::mutate(sample = url)

count <- ga.api.count(synthesis_id)

length <- ga.api.length(synthesis_id)

species_list <- ga.api.species.list(count)

# Add species information to count and length files ----
count_with_species <- count %>%
  mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
  left_join(., species_list)

length_with_species <- length %>%
  mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
  left_join(., species_list)

# Save raw data (if needed) ----
saveRDS(metadata, "output/geographe/metadata.RDS")
saveRDS(count_with_species, "output/geographe/count.RDS")
saveRDS(length_with_species, "output/geographe/length.RDS")

# Create abundance by size class ----
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
  dplyr::filter(!is.na(class))

# PLOTS ----

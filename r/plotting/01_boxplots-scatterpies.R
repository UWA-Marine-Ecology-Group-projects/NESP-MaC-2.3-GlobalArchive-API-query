###
# Project: SW Habitat & Fish
# Data:    Ningaloo & Geographe Habitat & Fish Syntheses
# Task:    Visualise data as boxplots and scatterpies
# Author:  Claude Spencer
# Date:    July 2023
##

# Clear the environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(scatterpie)

# Load the data
metadata <- readRDS("output/Geographe-bay_metadata.RDS") %>%
  dplyr::select(sample, longitude, latitude) %>%
  glimpse()

geo <- readRDS("output/Geographe-bay_length.RDS") %>%
  dplyr::mutate(fb_length_at_maturity = fb_length_at_maturity_cm * 10,
                scientific = paste(genus, species, sep = " ")) %>%
  dplyr::mutate(indicator.species = ifelse(scientific %in% c("Glaucosoma hebraicum",
                                                             "Chrysophrys auratus",
                                                             "Choerodon rubescens"), "Yes", "No")) %>%
  left_join(metadata) %>%
  glimpse()

test <- geo %>%
  dplyr::filter(indicator.species %in% "Yes")

metadata.length <- geo %>%
  distinct(sample) %>%
  glimpse()

fifty <- geo %>%
  dplyr::filter(indicator.species %in% "Yes") %>%
  dplyr::filter(length < (fb_length_at_maturity/2)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = "0-50") %>%
  dplyr::glimpse()

fiftyhunnit <- geo %>%
  dplyr::filter(indicator.species %in% "Yes") %>%
  dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = "50-100") %>%
  dplyr::glimpse()

hunnit <- geo %>%
  dplyr::filter(indicator.species %in% "Yes") %>%
  dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity*1.25)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = "100-125") %>%
  dplyr::glimpse()

onetwofive <- geo %>%
  dplyr::filter(indicator.species %in% "Yes") %>%
  dplyr::filter(length > (fb_length_at_maturity*1.25) & length < (fb_length_at_maturity*1.5)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = "125-150") %>%
  dplyr::glimpse()

onefiftyplus <- geo %>%
  dplyr::filter(indicator.species %in% "Yes") %>%
  dplyr::filter(length < (fb_length_at_maturity *1.5)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = "150+") %>%
  dplyr::glimpse()

greater.mat <- geo %>%
  dplyr::filter(indicator.species %in% "Yes") %>%
  dplyr::filter(length > fb_length_at_maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = ">Lm") %>%
  dplyr::glimpse()

smaller.mat <- geo %>%
  dplyr::filter(indicator.species %in% "Yes") %>%
  dplyr::filter(length < fb_length_at_maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(scientific = "<Lm") %>%
  dplyr::glimpse()

geo.length <- bind_rows(fifty, fiftyhunnit,
                        hunnit, onetwofive,
                        onefiftyplus, greater.mat,
                        smaller.mat) %>%
  glimpse()

# Boxplot for size of maturity - Geographe Bay Synthesis
ggplot(data = geo.length, aes(x = scientific, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.2, h = 0)) +
  labs(x = "Indicator species (size of maturity)", y = "Abundance") +
  scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

# Boxplot for CTI - Geographe Bay Synthesis
ggplot(data = geo, aes(x = subject_list_type, y = rls_thermal_niche)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.2, h = 0)) +
  labs(x = "Community Thermal Index", y = "Abundance") +
  theme_classic()

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

# Set synthesis
synthesis <- "Geographe-bay"
synthesis_id <- 14 # For Geographe Fish + Habitat Syntheses

# API call for Metadata ----
metadata <- ga.api.metadata(synthesis_id) %>%
  dplyr::filter(successful_length = TRUE)

# API call for Length ----
length <- ga.api.length(synthesis_id) %>%
  dplyr::left_join(metadata)

# Create Community Thermal Index (CTI) data from lengths + 3D points ----
cti <- length %>%
  dplyr::filter(!is.na(community_thermal_index)) %>% # to keep all fish (including those that do not have a CTI) turn this off with a #
  dplyr::select(-length_at_maturity) %>%
  glimpse()

# Save CTI data as a csv ----
write.csv(cti, paste0("data/tidy/", synthesis,"_CTI.csv"), row.names = F)

# Create plot ----
ggplot(data = cti %>% filter(!is.na(zone)), aes(x = zone, y = community_thermal_index)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "Zone", y = "Community Thermal Index (CTI)") +
  theme_classic() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

ggsave(filename = paste0("plots/", paste(synthesis,"community-thermal-index", "boxplot.png", sep = "_")),
       plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300)

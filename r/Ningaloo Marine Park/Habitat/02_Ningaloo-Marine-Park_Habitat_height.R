###
# Project: NESP MAC 2.3 GlobalArchive API Query
# Data:    Ningaloo Habitat Height Syntheses
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
synthesis <- "Ningaloo"
synthesis_id <- 17 # For Ningaloo Fish + Habitat Syntheses

# API call for Metadata ----
metadata <- ga.api.metadata(synthesis_id)

# API call for Habitat height ----
habitat_height <- ga.api.habitat.height(synthesis_id) %>%
  dplyr::select(sample, level_2, length, number) %>%
  dplyr::filter(!level_2 %in% "Unscorable") %>%
  left_join(metadata)

# Save out data
write.csv(habitat_height, paste0("data/tidy/", synthesis,"_habitat-height.csv"), row.names = F)

# Plot data
ggplot(data = habitat_height, aes(x = level_2, y = length)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = NULL, y = "Length (mm)") +
  theme_classic()

ggsave(filename = paste0("plots/", paste(synthesis,"habitat-height", "boxplot.png", sep = "_")),
       plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300)

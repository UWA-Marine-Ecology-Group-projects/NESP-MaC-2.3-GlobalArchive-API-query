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
library(scatterpie)
library(arrow)
library(terra)
sf_use_s2(F)
library(ggnewscale)

# Set cropping limit to speed up plotting
e <- ext(113, 114, -23, -22)

# Bring in functions for API call ----
# TODO turn this into a package.
source("r/00_Functions-for-API (do not run).R")
source("r/01_Functions-for-Plotting.R")

# Set synthesis
synthesis <- "Ningaloo"
synthesis_id <- 15 # For Ningaloo Fish + Habitat Syntheses

# API call for Metadata ----
metadata <- ga.api.metadata(synthesis_id)

# API call for Habitat ----
habitat <- ga.api.habitat(synthesis_id) %>%
  left_join(metadata) %>%
  dplyr::group_by(sample, level_2) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = level_2, values_from = count, values_fill = 0) %>%
  left_join(metadata)

write.csv(habitat, paste0("data/tidy/", synthesis,"_habitat.csv"),
          row.names = F)

# Make the scatterpies
ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = wampa, aes(fill = zonename), alpha = 1, color = NA, show.legend = F)+
  wampa_fills +
  new_scale_fill()+
  geom_sf(data = aumpa, aes(fill = ZoneName),alpha = 1, color = NA, show.legend = F) +
  aumpa_fills +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  new_scale_fill()+
  geom_scatterpie(aes(x = longitude, y = latitude), data = habitat,
                  cols = c("Substrate", "Sessile invertebrates","Sponges", "Cnidaria"),
                  pie_scale = 1, colour = NA) +
  labs(fill = "Habitat", x = 'Longitude', y = 'Latitude')+
  hab_cols +
  coord_sf(xlim = c(min(habitat$longitude), max(habitat$longitude)),
           ylim = c(min(habitat$latitude), max(habitat$latitude)))+
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = paste0("plots/", paste(synthesis,"habitat",
                                         "scatterpies.png", sep = "_")),
       units = "in", dpi = 300, height = 8, width = 6)

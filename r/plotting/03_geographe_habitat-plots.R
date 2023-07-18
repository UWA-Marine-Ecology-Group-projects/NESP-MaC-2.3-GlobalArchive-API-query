###
# Project: API query
# Data:    Geographe Habitat & Fish Syntheses
# Task:    Visualise habitat as scatterpies
# Author:  Claude Spencer
# Date:    July 2023
##

# Clear the environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(scatterpie)
library(arrow)
library(scatterpie)

# Set synthesis
synthesis <- "Geographe-bay"

# Load the data
metadata <- readRDS(paste0("output/", synthesis, "_metadata.RDS")) %>%
  dplyr::select(sample, longitude, latitude) %>%
  glimpse()

habitat <- readRDS(paste0("output/", synthesis, "_habitat.RDS")) %>%
  left_join(metadata) %>%
  dplyr::select(sample, longitude, latitude, level_2, count) %>%
  dplyr::group_by(sample, longitude, latitude, level_2) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = level_2, values_from = count) %>%
  dplyr::mutate(grouping = factor(1:nrow(.)),
                Cnidaria = replace_na(Cnidaria, 0),
                Substrate = replace_na(Substrate, 0),
                Seagrasses = replace_na(Seagrasses, 0),
                Macroalgae = replace_na(Macroalgae, 0),) %>%
  glimpse()

# Set up plot elements
# Set plotting colours
hab_cols <- scale_fill_manual(values = c("Cnidaria" = "plum",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Seagrasses" = "forestgreen",
                                         "Substrate" = "wheat"))

# Shapefile of australia


# Make the scatterpies
ggplot() +
  # geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  # geom_sf(data = wampa,fill = "#bfd054", alpha = 2/5, color = NA)+
  # wampa_cols +
  # labs(fill = "State Marine Parks")+
  # new_scale_fill()+
  # geom_sf(data = aumpa, fill = "#7bbc63",alpha = 2/5, color = NA) +
  # labs(fill = "Australian Marine Parks")+
  # nmpa_cols+
  # geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  # new_scale_fill()+
  geom_scatterpie(aes(x = longitude, y = latitude, group = grouping), data = habitat,
                  cols = c("Substrate","Macroalgae","Seagrasses",
                           "Cnidaria"),
                  pie_scale = 0.45, colour = NA) +
  labs(fill = "Habitat", x = 'Longitude', y = 'Latitude')+
  hab_cols +
  coord_sf(xlim = c(min(habitat$longitude), max(habitat$longitude)),
           ylim = c(min(habitat$latitude), max(habitat$latitude)))+
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

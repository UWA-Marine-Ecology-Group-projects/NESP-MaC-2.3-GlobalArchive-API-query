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
library(terra)
library(sf)
sf_use_s2(F)
library(ggnewscale)

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

write.csv(habitat, paste0("data/tidy/", synthesis,"_habitat.csv"),
          row.names = F)

# Set up plot elements
# Set plotting colours
hab_cols <- scale_fill_manual(values = c("Cnidaria" = "plum",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Seagrasses" = "forestgreen",
                                         "Substrate" = "wheat"))

# Set cropping limit to speed up plotting
e <- ext(115, 116, -34, -33)

# Shapefile of australia
aus <- st_read("data/spatial/cstauscd_r.mif", crs = 4283) %>%
  st_crop(e) %>%
  st_transform(4326) %>%
  dplyr::filter(!FEAT_CODE %in% "sea")

# Shapefile of australian marine parks
aumpa <- st_read("data/spatial/AustraliaNetworkMarineParks.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

aumpa_fills <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",# Commonwealth MPA colours
                                          # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          # "Recreational Use Zone" = "#ffb36b",
                                          # "Sanctuary Zone" = "#f7c0d8",
                                          # "Special Purpose Zone" = "#6daff4",
                                          # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                          "Special Purpose Zone (Mining Exclusion)" = "#368ac1"
))

# Shapefile of state marine parks
wampa <- st_read("data/spatial/WA_MPA_2020.shp", crs = 7844) %>%
  st_crop(e) %>%
  st_transform(4326) %>%
  dplyr::mutate(zonename = str_replace_all(ZONE_TYPE, " \\s*\\([^\\)]+\\)", "")) %>%
  glimpse()

wampa_fills <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           # "Marine Nature Reserve" = "#bfd054",
                                           # "Conservation Area" = "#b3a63d",
                                           # "Habitat Protection Zone" = "#fffbcc",# State MPA colours
                                           # "Fish Habitat Protection Area" = "#fbff85",
                                           # "National Park Zone" = "#a4d194",
                                           "General Use Zone" = "#bddde1",
                                           # "Recreation Zone" = "#f4e952"
                                           "Special Purpose Zone" = "#c5bcc9"
                                           # "Reef Observation Area" = "#ddccff",
                                           # "Marine Management Area" = "#b7cfe1"
))

unique(wampa$zonename)

# Coastal waters limit
cwatr <- st_read("data/spatial/amb_coastal_waters_limit.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

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
  geom_scatterpie(aes(x = longitude, y = latitude, group = grouping), data = habitat,
                  cols = c("Substrate","Macroalgae","Seagrasses",
                           "Cnidaria"),
                  pie_scale = 0.45, colour = NA) +
  labs(fill = "Habitat", x = 'Longitude', y = 'Latitude')+
  hab_cols +
  coord_sf(xlim = c(min(habitat$longitude), max(habitat$longitude)),
           ylim = c(min(habitat$latitude), max(habitat$latitude)))+
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename = paste0("plots/", paste(synthesis,"habitat",
                                         "scatterpies.png", sep = "_")),
       units = "in", dpi = 300, height = 6, width = 8)

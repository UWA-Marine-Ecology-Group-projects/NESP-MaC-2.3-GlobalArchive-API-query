###
# Project: API query
# Data:    Ningaloo Habitat & Fish Syntheses
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
synthesis <- "Ningaloo benthic"

# Load the data
metadata <- readRDS(paste0("output/", synthesis, "_metadata.RDS")) %>%
  dplyr::select(sample, longitude, latitude) %>%
  glimpse()

length <- readRDS(paste0("output/", synthesis, "_habitat-length.RDS")) %>%
  dplyr::select(sample, level_2, length, number) %>%
  dplyr::filter(!level_2 %in% "Unscorable") %>%
  glimpse()

# Save out data
write.csv(length, paste0("data/tidy/", synthesis,"_habitat-height.csv"),
          row.names = F)

# Plot data
ggplot(data = length,
       aes(x = level_2, y = length)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = NULL, y = "Length (mm)") +
  theme_classic()
ggsave(filename = paste0("plots/", paste(synthesis,"habitat-height",
                                         "boxplot.png", sep = "_")),
       plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300)

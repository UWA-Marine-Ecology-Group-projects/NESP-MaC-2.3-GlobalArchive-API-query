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
  left_join(metadata) %>%
  dplyr::select(sample, length, number, scientific,
                fb_length_at_maturity, subject_list_type,
                rls_thermal_niche) %>%
  glimpse()

geo.indicators <- geo %>%
  dplyr::select(sample, length, number, scientific, fb_length_at_maturity) %>%
  dplyr::filter(scientific %in% c("Choerodon rubescens",
                                  "Chrysophrys auratus",
                                  "Glaucosoma hebraicum")) %>%
  dplyr::mutate(fb_length_at_maturity = ifelse(scientific %in% "Choerodon rubescens", 479, fb_length_at_maturity)) %>%
  glimpse()

metadata.length <- geo %>%
  distinct(sample) %>%
  glimpse()

species <- geo %>%
  dplyr::select(sample, scientific, fb_length_at_maturity) %>%
  dplyr::filter(scientific %in% c("Choerodon rubescens",
                                  "Chrysophrys auratus",
                                  "Glaucosoma hebraicum")) %>%
  dplyr::mutate(fb_length_at_maturity = ifelse(scientific %in% "Choerodon rubescens", 479, fb_length_at_maturity)) %>%
  right_join(metadata.length) %>%
  complete(sample, nesting(scientific, fb_length_at_maturity)) %>%
  dplyr::filter(!is.na(scientific)) %>%
  glimpse()

sizeclass_by_abundance <- function(data, metadata, species, min, max, label) {
  # For the function to run you need
  # Metadata - dataframe with 1 column of all samples
  # Species - dataframe with each indicator species for all samples
  tempdat <- data %>%
    dplyr::filter(length > (fb_length_at_maturity * min) & length < (fb_length_at_maturity * max)) %>%
    dplyr::group_by(sample, scientific) %>%
    dplyr::summarise(number = sum(number)) %>%
    right_join(species) %>%
    dplyr::mutate(number = replace_na(number, 0)) %>%
    dplyr::mutate(size.class = label) %>%
    ungroup()

  tempdat <- data %>%
    dplyr::filter(length > (fb_length_at_maturity * min) & length < (fb_length_at_maturity * max)) %>%
    dplyr::group_by(sample) %>%
    dplyr::summarise(number = sum(number)) %>%
    right_join(metadata) %>%
    dplyr::mutate(number = replace_na(number, 0)) %>%
    dplyr::mutate(size.class = label,
                  scientific = "all.indicators") %>%
    ungroup() %>%
    bind_rows(tempdat)
  assign(label, tempdat, envir = .GlobalEnv)

}

sizeclass_by_abundance(geo.indicators, metadata.length, species, 0, 0.5, "0-50")
sizeclass_by_abundance(geo.indicators, metadata.length, species, 0.5, 1, "50-100")
sizeclass_by_abundance(geo.indicators, metadata.length, species, 1, 1.25, "100-125")
sizeclass_by_abundance(geo.indicators, metadata.length, species, 1.25, 1.5, "125-150")
sizeclass_by_abundance(geo.indicators, metadata.length, species, 1.5, Inf, ">150")
sizeclass_by_abundance(geo.indicators, metadata.length, species, 0, 1.0, "<Lm")
sizeclass_by_abundance(geo.indicators, metadata.length, species, 1.0, Inf, ">Lm")

geo.length <- bind_rows(`0-50`, `50-100`,
                        `100-125`, `125-150`,
                        `>150`, `<Lm`,
                        `>Lm`) %>%
  dplyr::mutate(size.class = factor(size.class,
                                    levels = c("0-50","50-100","100-125",
                                               "125-150","150+",">Lm", "<Lm"))) %>%
  glimpse()

# Boxplot for size of maturity - Geographe Bay Synthesis
# Greater than and less than size of 50% maturity
# All species
plot_size_of_maturity <- function(data, size.classes, scientific.names) {
  # data = Dataframe
  # Vector of size classes
  # Vector of scientific to include
  temp.plot <- ggplot(data = data %>%
           dplyr::filter(size.class %in% size.classes &
                           scientific %in% scientific.names),
         aes(x = size.class, y = number)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
    labs(x = "All indicator species (size of maturity)", y = "Abundance") +
    theme_classic()

}

ggplot(data = geo.length %>%
         dplyr::filter(size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "all.indicators"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

ggplot(data = geo.length %>%
         dplyr::filter(!size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "all.indicators"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.1, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

# Each species
# Baldie
ggplot(data = geo.length %>%
         dplyr::filter(size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "Choerodon rubescens"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

ggplot(data = geo.length %>%
         dplyr::filter(!size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "Choerodon rubescens"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.1, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

# Dhuey
ggplot(data = geo.length %>%
         dplyr::filter(size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "Glaucosoma hebraicum"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

ggplot(data = geo.length %>%
         dplyr::filter(!size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "Glaucosoma hebraicum"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.1, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

# Pinkie
ggplot(data = geo.length %>%
         dplyr::filter(size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "Chrysophrys auratus"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

ggplot(data = geo.length %>%
         dplyr::filter(!size.class %in% c(">Lm", "<Lm") &
                         scientific %in% "Chrysophrys auratus"),
       aes(x = size.class, y = number)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.1, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "All indicator species (size of maturity)", y = "Abundance") +
  # scale_y_continuous(limits = c(0, 6)) +
  theme_classic()

# Boxplot for CTI - Geographe Bay Synthesis (all species)
ggplot(data = geo, aes(x = subject_list_type, y = rls_thermal_niche)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.05, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = "Community Thermal Index", y = "Abundance") +
  theme_classic() +
  theme(axis.text.x = element_blank())


# fifty <- geo.indicators %>%
#   dplyr::filter(length < (fb_length_at_maturity/2)) %>%
#   dplyr::group_by(sample, scientific) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "0-50") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# fifty.all <- geo.indicators %>%
#   dplyr::filter(length < (fb_length_at_maturity/2)) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "0-50",
#                 scientific = "all.indicators") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# fifty <- bind_rows(fifty.all, fifty) %>%
#   glimpse()
#
# fiftyhunnit <- geo.indicators %>%
#   dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity)) %>%
#   dplyr::group_by(sample, scientific) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "50-100") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# fiftyhunnit.all <- geo.indicators %>%
#   dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity)) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "50-100",
#                 scientific = "all.indicators") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# fiftyhunnit <- bind_rows(fiftyhunnit.all, fiftyhunnit) %>%
#   glimpse()
#
# hunnit <- geo.indicators %>%
#   dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity*1.25)) %>%
#   dplyr::group_by(sample, scientific) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "100-125") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# hunnit.all <- geo.indicators %>%
#   dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity*1.25)) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "100-125",
#                 scientific = "all.indicators") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# hunnit <- bind_rows(hunnit, hunnit.all) %>%
#   glimpse()
#
# onetwofive <- geo.indicators %>%
#   dplyr::filter(length > (fb_length_at_maturity*1.25) & length < (fb_length_at_maturity*1.5)) %>%
#   dplyr::group_by(sample, scientific) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "125-150") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# onetwofive.all <- geo.indicators %>%
#   dplyr::filter(length > (fb_length_at_maturity*1.25) & length < (fb_length_at_maturity*1.5)) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "125-150",
#                 scientific = "all.indicators") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# onetwofive <- bind_rows(onetwofive, onetwofive.all) %>%
#   glimpse()
#
# onefiftyplus <- geo.indicators %>%
#   dplyr::filter(length < (fb_length_at_maturity *1.5)) %>%
#   dplyr::group_by(sample, scientific) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "150+") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# onefiftyplus.all <- geo.indicators %>%
#   dplyr::filter(length < (fb_length_at_maturity *1.5)) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "150+",
#                 scientific = "all.indicators") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# onefiftyplus <- bind_rows(onefiftyplus, onefiftyplus.all) %>%
#   glimpse()
#
# greater.mat <- geo.indicators %>%
#   dplyr::filter(length > fb_length_at_maturity) %>%
#   dplyr::group_by(sample, scientific) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = ">Lm") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# greater.mat.all <- geo.indicators %>%
#   dplyr::filter(length > fb_length_at_maturity) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = ">Lm",
#                 scientific = "all.indicators") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# greater.mat <- bind_rows(greater.mat, greater.mat.all) %>%
#   glimpse()
#
# smaller.mat <- geo.indicators %>%
#   dplyr::filter(length < fb_length_at_maturity) %>%
#   dplyr::group_by(sample, scientific) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "<Lm") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# smaller.mat.all <- geo.indicators %>%
#   dplyr::filter(length < fb_length_at_maturity) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   right_join(metadata.length) %>%
#   dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
#   dplyr::mutate(size.class = "<Lm",
#                 scientific = "all.indicators") %>%
#   ungroup() %>%
#   dplyr::glimpse()
#
# smaller.mat <- bind_rows(smaller.mat, smaller.mat.all) %>%
#   glimpse()

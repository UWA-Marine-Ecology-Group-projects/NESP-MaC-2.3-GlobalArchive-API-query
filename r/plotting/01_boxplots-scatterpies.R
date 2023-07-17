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
  glimpse()

metadata.length <- geo %>%
  distinct(sample) %>%
  glimpse()

fifty <- geo.indicators %>%
  dplyr::filter(length < (fb_length_at_maturity/2)) %>%
  dplyr::group_by(sample, scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "0-50") %>%
  ungroup() %>%
  dplyr::glimpse()

fifty.all <- geo.indicators %>%
  dplyr::filter(length < (fb_length_at_maturity/2)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "0-50",
                scientific = "all.indicators") %>%
  ungroup() %>%
  dplyr::glimpse()

fifty <- bind_rows(fifty.all, fifty) %>%
  glimpse()

fiftyhunnit <- geo.indicators %>%
  dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity)) %>%
  dplyr::group_by(sample, scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "50-100") %>%
  ungroup() %>%
  dplyr::glimpse()

fiftyhunnit.all <- geo.indicators %>%
  dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "50-100",
                scientific = "all.indicators") %>%
  ungroup() %>%
  dplyr::glimpse()

fiftyhunnit <- bind_rows(fiftyhunnit.all, fiftyhunnit) %>%
  glimpse()

hunnit <- geo.indicators %>%
  dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity*1.25)) %>%
  dplyr::group_by(sample, scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "100-125") %>%
  ungroup() %>%
  dplyr::glimpse()

hunnit.all <- geo.indicators %>%
  dplyr::filter(length > (fb_length_at_maturity/2) & length < (fb_length_at_maturity*1.25)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "100-125",
                scientific = "all.indicators") %>%
  ungroup() %>%
  dplyr::glimpse()

hunnit <- bind_rows(hunnit, hunnit.all) %>%
  glimpse()

onetwofive <- geo.indicators %>%
  dplyr::filter(length > (fb_length_at_maturity*1.25) & length < (fb_length_at_maturity*1.5)) %>%
  dplyr::group_by(sample, scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "125-150") %>%
  ungroup() %>%
  dplyr::glimpse()

onetwofive.all <- geo.indicators %>%
  dplyr::filter(length > (fb_length_at_maturity*1.25) & length < (fb_length_at_maturity*1.5)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "125-150",
                scientific = "all.indicators") %>%
  ungroup() %>%
  dplyr::glimpse()

onetwofive <- bind_rows(onetwofive, onetwofive.all) %>%
  glimpse()

onefiftyplus <- geo.indicators %>%
  dplyr::filter(length < (fb_length_at_maturity *1.5)) %>%
  dplyr::group_by(sample, scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "150+") %>%
  ungroup() %>%
  dplyr::glimpse()

onefiftyplus.all <- geo.indicators %>%
  dplyr::filter(length < (fb_length_at_maturity *1.5)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "150+",
                scientific = "all.indicators") %>%
  ungroup() %>%
  dplyr::glimpse()

onefiftyplus <- bind_rows(onefiftyplus, onefiftyplus.all) %>%
  glimpse()

greater.mat <- geo.indicators %>%
  dplyr::filter(length > fb_length_at_maturity) %>%
  dplyr::group_by(sample, scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = ">Lm") %>%
  ungroup() %>%
  dplyr::glimpse()

greater.mat.all <- geo.indicators %>%
  dplyr::filter(length > fb_length_at_maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = ">Lm",
                scientific = "all.indicators") %>%
  ungroup() %>%
  dplyr::glimpse()

greater.mat <- bind_rows(greater.mat, greater.mat.all) %>%
  glimpse()

smaller.mat <- geo.indicators %>%
  dplyr::filter(length < fb_length_at_maturity) %>%
  dplyr::group_by(sample, scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "<Lm") %>%
  ungroup() %>%
  dplyr::glimpse()

smaller.mat.all <- geo.indicators %>%
  dplyr::filter(length < fb_length_at_maturity) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(size.class = "<Lm",
                scientific = "all.indicators") %>%
  ungroup() %>%
  dplyr::glimpse()

smaller.mat <- bind_rows(smaller.mat, smaller.mat.all) %>%
  glimpse()

geo.length <- bind_rows(fifty, fiftyhunnit,
                        hunnit, onetwofive,
                        onefiftyplus, greater.mat,
                        smaller.mat) %>%
  dplyr::mutate(size.class = factor(size.class,
                                    levels = c("0-50","50-100","100-125",
                                               "125-150","150+",">Lm", "<Lm"))) %>%
  glimpse()

# Boxplot for size of maturity - Geographe Bay Synthesis
# Greater than and less than size of 50% maturity
# All species
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

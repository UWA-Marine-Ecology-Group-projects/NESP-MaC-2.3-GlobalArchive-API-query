###
# Project: API query
# Data:    Geographe Habitat & Fish Syntheses
# Task:    Visualise Lm & CTI data as boxplots
# Author:  Claude Spencer
# Date:    July 2023
##

# Clear the environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(scatterpie)
library(arrow)

# Set synthesis
synthesis <- "Geographe-bay"

# Load the data
metadata <- readRDS(paste0("output/", synthesis, "_metadata.RDS")) %>%
  dplyr::select(sample, longitude, latitude) %>%
  glimpse()

length.raw <- readRDS(paste0("output/", synthesis, "_length.RDS")) %>%
  dplyr::mutate(fb_length_at_maturity = fb_length_at_maturity_cm * 10,
                scientific = paste(genus, species, sep = " ")) %>%
  left_join(metadata) %>%
  dplyr::select(sample, length, number, scientific,
                fb_length_at_maturity,
                rls_thermal_niche) %>%
  glimpse()

cti <- length.raw %>%
  dplyr::select(sample, scientific, rls_thermal_niche) %>%
  dplyr::filter(!is.na(rls_thermal_niche)) %>%
  glimpse()

write.csv(cti, paste0("data/tidy/", synthesis,"_cti.csv"),
          row.names = F)

length.indicators <- length.raw %>%
  dplyr::select(sample, length, number, scientific, fb_length_at_maturity) %>%
  dplyr::filter(scientific %in% c("Choerodon rubescens",
                                  "Chrysophrys auratus",
                                  "Glaucosoma hebraicum")) %>%
  # dplyr::mutate(fb_length_at_maturity = ifelse(scientific %in% "Choerodon rubescens", 479, fb_length_at_maturity)) %>%
  glimpse()

metadata.length <- length.raw %>%
  distinct(sample) %>%
  glimpse()

species <- length.raw %>%
  dplyr::select(sample, scientific, fb_length_at_maturity) %>%
  dplyr::filter(scientific %in% c("Choerodon rubescens",
                                  "Chrysophrys auratus",
                                  "Glaucosoma hebraicum")) %>%
  # dplyr::mutate(fb_length_at_maturity = ifelse(scientific %in% "Choerodon rubescens", 479, fb_length_at_maturity)) %>%
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

sizeclass_by_abundance(length.indicators, metadata.length, species, 0, 0.5, "0-50")
sizeclass_by_abundance(length.indicators, metadata.length, species, 0.5, 1, "50-100")
sizeclass_by_abundance(length.indicators, metadata.length, species, 1, 1.25, "100-125")
sizeclass_by_abundance(length.indicators, metadata.length, species, 1.25, 1.5, "125-150")
sizeclass_by_abundance(length.indicators, metadata.length, species, 1.5, Inf, ">150")
sizeclass_by_abundance(length.indicators, metadata.length, species, 0, 1.0, "<Lm")
sizeclass_by_abundance(length.indicators, metadata.length, species, 1.0, Inf, ">Lm")

length <- bind_rows(`0-50`, `50-100`,
                    `100-125`, `125-150`,
                    `>150`, `<Lm`,
                    `>Lm`) %>%
  dplyr::mutate(size.class = factor(size.class,
                                    levels = c("0-50","50-100","100-125",
                                               "125-150","150+",">Lm", "<Lm"))) %>%
  glimpse()

write.csv(length, file = paste0("data/tidy/", synthesis, "_size-of-maturity.csv"),
          row.names = F)

# Boxplot for size of maturity
plot_size_of_maturity <- function(dat, plot.types, scientific.names, tidy.name) {
  # data = Dataframe
  # Vector of size classes
  # Vector of responses to include
  if (plot.types == 1) {size.classes = c(">Lm", "<Lm")}
  if (plot.types == 2) {size.classes = c("0-50", "50-100",
                                         "100-125", "125-150",
                                         "150+")}
  ggplot(data = dat %>%
           dplyr::filter(size.class %in% size.classes &
                           scientific %in% scientific.names),
         aes(x = size.class, y = number)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
    labs(x = tidy.name, y = "Abundance") +
    theme_classic()

  if (plot.types == 1) {
    ggsave(filename = paste0("plots/", paste(synthesis, "length.maturity", scientific.names,
                                             "boxplot.png", sep = "_")),
           plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300)
  }
  if (plot.types == 2) {
    ggsave(filename = paste0("plots/", paste(synthesis,"maturity.bins", scientific.names,
                                             "boxplot.png", sep = "_")),
           plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300)
  }
}

plot_size_of_maturity(length, plot.types =  1,
                      scientific.names = "all.indicators",
                      tidy.name = "All indicator species")

plot_size_of_maturity(length, plot.types = 2,
                      scientific.names = "all.indicators",
                      tidy.name = "All indicator species")

plot_size_of_maturity(length, plot.types = 1,
                      scientific.names = "Glaucosoma hebraicum",
                      tidy.name = "Glaucosoma hebraicum")

plot_size_of_maturity(length, plot.types = 2,
                      scientific.names = "Glaucosoma hebraicum",
                      tidy.name = "Glaucosoma hebraicum")

plot_size_of_maturity(length, plot.types = 1,
                      scientific.names = "Choerodon rubescens",
                      tidy.name = "Choerodon rubescens")

plot_size_of_maturity(length, plot.types = 2,
                      scientific.names = "Choerodon rubescens",
                      tidy.name = "Choerodon rubescens")

plot_size_of_maturity(length, plot.types = 1,
                      scientific.names = "Chrysophrys auratus",
                      tidy.name = "Chrysophrys auratus")

plot_size_of_maturity(length, plot.types = 2,
                      scientific.names = "Chrysophrys auratus",
                      tidy.name = "Chrysophrys auratus")

ggplot(data = length.raw %>% dplyr::mutate(title = "Community Thermal Index"),
       aes(x = title, y = rls_thermal_niche)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
  labs(x = NULL, y = "Community Thermal Index (CTI)") +
  theme_classic()

ggsave(filename = paste0("plots/", paste(synthesis,"community-thermal-index",
                                         "boxplot.png", sep = "_")),
       plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300)


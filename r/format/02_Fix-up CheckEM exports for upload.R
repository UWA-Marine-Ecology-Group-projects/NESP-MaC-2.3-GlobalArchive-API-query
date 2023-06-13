library(tidyverse)
library(googlesheets4)
library(GlobalArchive)

options(gargle_oauth_cache = ".secrets")
googlesheets4::gs4_auth()

lh.url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit#gid=825736197"

lh <- googlesheets4::read_sheet(lh.url) %>% ga.clean.names()

caabs <- lh %>% dplyr::select(caab, family, genus, species)

# Read in data ----
metadata <- read_csv("data/syntheses/geographe/2014-12_Geographe.Bay_stereoBRUVs_metadata.csv") %>%
  dplyr::mutate(observer.count = if_else(successful.count %in% "Yes" & is.na(observer.count), "Unknown", observer.count))%>%
  dplyr::mutate(observer.length = if_else(successful.length %in% "Yes" & is.na(observer.length), "Unknown", observer.length))

count <- read_csv("data/syntheses/geographe/2014-12_Geographe.Bay_stereoBRUVs_count.csv") %>%
  dplyr::mutate(family = if_else(genus %in% "Heteroscarus", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% c("Anoplocapros", "Caprichthys"), "Aracanidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% c("Trygonorrhina","Aptychotrema"), "Rhinobatidae", family)) %>%
  dplyr::rename(original.caab = caab) %>%
  left_join(., caabs) %>%
  dplyr::mutate(caab = if_else(!is.na(caab), caab, original.caab)) %>%
  dplyr::select(-c(original.caab)) %>%
  group_by(campaignid, sample, family, genus, species, caab) %>%
  slice_max(count)

length <- read_csv("data/syntheses/geographe/2014-12_Geographe.Bay_stereoBRUVs_length.csv")%>%
  dplyr::mutate(family = if_else(genus %in% "Heteroscarus", "Labridae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% c("Anoplocapros", "Caprichthys"), "Aracanidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% c("Trygonorrhina","Aptychotrema"), "Rhinobatidae", family)) %>%
  dplyr::rename(original.caab = caab) %>%
  left_join(., caabs) %>%
  dplyr::mutate(caab = if_else(!is.na(caab), caab, original.caab)) %>%
  dplyr::select(-c(original.caab))

# Missing metadata ----
missing.depth <- metadata %>% filter(is.na(depth))
missing.date.time <- metadata %>% filter(is.na(date.time))

count.not.in.lh <- anti_join(count, lh) %>% distinct(family, genus, species, caab)
length.not.in.lh <- anti_join(length, lh) %>% distinct(family, genus, species, caab)

# Check for Duplicate combinations of ['caab', 'campaignid', 'sample'] in count files. ----
check.duplicates <- count %>%
  dplyr::group_by(caab, campaignid, sample) %>%
  dplyr::summarise(n = n()) %>%
  filter(n > 1)



write.csv(metadata, "data/syntheses/geographe/2014-12_Geographe.Bay_stereoBRUVs_metadata.csv", row.names = FALSE)
write.csv(count, "data/syntheses/geographe/2014-12_Geographe.Bay_stereoBRUVs_count.csv", row.names = FALSE)
write.csv(length, "data/syntheses/geographe/2014-12_Geographe.Bay_stereoBRUVs_length.csv", row.names = FALSE)

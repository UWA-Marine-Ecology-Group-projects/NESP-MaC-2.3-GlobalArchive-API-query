# libraries ---
library(tidyverse)
library(GlobalArchive)
library(googlesheets4)
library(RCurl)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
gargle::gargle_oauth_cache()
googlesheets4::gs4_auth()
2

# Read in rosetta stone ----
url <- "https://docs.google.com/spreadsheets/d/1tcvHnD8LtPmjro8eOMdOS6k6HKdND86mMAOk6AS_gfc/edit?usp=sharing"

stone <- read_sheet(url, sheet = "rosetta stone") %>%
  ga.clean.names() %>%
  dplyr::rename(clean.code = code) %>%
  dplyr::select(-c(schema.name, notes)) %>%
  distinct()

# download current one from github here:
# https://github.com/GlobalArchiveManual/annotation-schema/tree/main/output/habitat
levels <- read_csv("data/benthic.annotation.schema.forward.facing.20230714.135113.csv", col_types = cols(.default = "c")) %>%
  dplyr::rename(code = CAAB_code) %>%
  dplyr::mutate(code = as.character(code)) %>%
  dplyr::select(-c(qualifiers, Parent_CAAB))

# Read in habitat data to clean
data <- read.delim("data/raw/ningaloo/2022-05_PtCloates_stereo-BRUVS_Forwards_Dot Point Measurements.txt", skip = 4, na.strings = "") %>%
  ga.clean.names() %>%
  dplyr::select(opcode, image.row, image.col, broad, morphology, type, code) %>% # Keep period in for BOSS
  dplyr::mutate(code = as.character(code)) %>%
  dplyr::filter(!is.na(broad)) # remove not annotated points

# Join together
data.with.stone <- left_join(data, stone)

find.missing <- data.with.stone %>% dplyr::filter(is.na(clean.code)) # none missing
codes.dont.match <- data.with.stone %>% dplyr::filter(!code %in% clean.code) # some original codes are NA

# Now data has correct CAAB code can join with the correct schema (L1 - L5)
data.with.levels <- data.with.stone %>%
  dplyr::mutate(code = clean.code) %>%
  dplyr::left_join(levels) %>%
  dplyr::select(-c(broad, morphology, type, fine, clean.code))

missing.level <- data.with.levels %>%
  dplyr::filter(is.na(level_1)) # none = good

# Summarise data for upload
# need:
# campaignid, sample, code, level1, level2, level3, level4, level5, level6, level7, level8, family, genus, species, count

summarised <- data.with.levels %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(opcode, code, level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8, family, genus, species) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::rename(sample = opcode) %>%
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
  dplyr::rename(caab_code = code)

names(data.with.levels)

write.csv(summarised, "data/syntheses/ningaloo/2022-05_PtCloates_stereo-BRUVS_habitat.csv")

# Habitat with lengths ----
lengths <- read_delim("data/raw/ningaloo/habitat height/2022-05-PtCloates-BOSS_Lengths.txt", col_types = cols(.default = "c"))
points <- read_delim("data/raw/ningaloo/habitat height/2022-05-PtCloates-BOSS_3DPoints.txt", col_types = cols(.default = "c"))

heights <- bind_rows(lengths, points) %>%
  ga.clean.names() %>%
  dplyr::rename(sample = period) %>%
  dplyr::filter(!comment %in% c("Sync")) %>%
  dplyr::mutate(new.height = case_when(
    attribute9 == "10-20" ~ "10",
    attribute9 == "20-40" ~ "20",
    attribute9 == "40-60" ~ "40",
    attribute9 == "60-100" ~ "60",
    attribute9 == ">100" ~ "100"
  )) %>%
  dplyr::mutate(length = if_else(is.na(length), new.height, length)) %>%

  dplyr::mutate(code = if_else(family %in% "Cnidaria > Hydroids" & is.na(genus), "11001000", code)) %>% # add codes for two that are missing
  dplyr::mutate(code = if_else(family %in% "Echinoderms > Feather Stars" & is.na(genus), "25001000", code)) %>% # add codes for two that are missing
  dplyr::mutate(original.name = paste(family, genus, species)) %>% # For testing
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BOSS") %>%
  dplyr::select(campaignid, sample, code, length, number, range, rms, precision)

# Add habitat levels ----
heights.with.levels <- left_join(heights, levels)

# Format BOSS metadata ----
labsheets <- "https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing"

naked <- read_sheet(labsheets, sheet = "2022-05_PtCloates_Naked-BOSS") %>%
  dplyr::select(Sample, Latitude, Longitude, Date.time, Site, Location, Status, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment) %>%
  dplyr::mutate(Sample = as.character(Sample)) %>%
  glimpse()

squid <- read_sheet(labsheets, sheet = "2022-05_PtCloates_Squid-BOSS") %>%
  dplyr::select(Sample, Latitude, Longitude, Date.time, Site, Location, Status, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment) %>%
  dplyr::mutate(Sample = as.character(Sample)) %>%
  glimpse()

height.metadata <- bind_rows(naked, squid) %>%
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BOSS") %>%
  ga.clean.names()

duplicates <- height.metadata %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n = n()) # none = ok

write.csv(heights.with.levels, "data/syntheses/habitat height/2022-05_PtCloates_stereo-BOSS_habitat-lengths.csv")
write.csv(height.metadata, "data/syntheses/habitat height/2022-05_PtCloates_stereo-BOSS_metadata.csv")













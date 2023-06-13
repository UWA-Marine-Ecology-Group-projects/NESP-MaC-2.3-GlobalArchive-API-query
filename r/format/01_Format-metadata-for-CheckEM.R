# libraries ---
library(tidyverse)

# Geographe needed new date.time and observer.length columns ----
# geographe <- read_csv("data/raw/archive/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv") %>%
#   dplyr::rename(Observer.length = Length.analyst) %>%
#   dplyr::rename(Observer.count = Observer) %>%
#   dplyr::mutate(Comment = paste(Video.notes, Notes)) %>%
#   dplyr::mutate(Date = paste("2014-12", str_sub(Date, 1, 2), sep = "-")) %>%
#   dplyr::mutate(Date.time = paste(Date, "T", Time, "+08:00", sep = "")) %>%
#   dplyr::select(Sample, Latitude, Longitude, Status, Date.time, Site, Location, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment) %>%
#   glimpse()
#
# write.csv(geographe, "data/raw/geographe/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv", row.names = FALSE)

# South-west corner


# Ningaloo
charlottes <- read_csv("data/raw/archive/2019-08_Ningaloo_stereo-BRUVs_Metadata.csv") %>%
  dplyr::mutate(Date = paste("2019-08", str_sub(Date, 1, 2), sep = "-")) %>%
  dplyr::mutate(Date.time = paste(Date, "T", Time, "+08:00", sep = "")) %>%
  dplyr::rename(Observer.count = Observer) %>%
  dplyr::rename(Observer.length = Length.analyst) %>%
  dplyr::rename(Comment = Notes) %>%
  dplyr::select(Sample, Latitude, Longitude, Status, Date.time, Site, Location, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment)

names(charlottes)
unique(charlottes$Date)
unique(charlottes$Time)
write.csv(charlottes, "data/raw/ningaloo/2019-08_Ningaloo_stereo-BRUVs_Metadata.csv", row.names = FALSE)

twenty1 <- read_csv("data/raw/archive/2021-05_PtCloates_stereo-BRUVS_Metadata.csv") %>%
  dplyr::mutate(Date = paste("2021-05", str_sub(Date, 7, 8), sep = "-")) %>%
  dplyr::mutate(Date.time = paste(Date, "T", Time, "+08:00", sep = "")) %>%
  dplyr::rename(Observer.count = Observer) %>%
  dplyr::rename(Observer.length = Length.analyst) %>%
  dplyr::rename(Comment = Notes) %>%
  dplyr::select(Sample, Latitude, Longitude, Status, Date.time, Site, Location, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment)

names(twenty1)
write.csv(twenty1, "data/raw/ningaloo/2021-05_PtCloates_stereo-BRUVS_Metadata.csv", row.names = FALSE)

twenty2 <- read_csv("data/raw/archive/2022-05_PtCloates_stereo-BRUVS_Metadata.csv") %>%
  dplyr::mutate(Date = paste("2022-05", str_sub(Date, 7, 8), sep = "-")) %>%
  dplyr::mutate(Date.time = paste(Date, "T", Time, "+08:00", sep = "")) %>%
  dplyr::rename(Observer.count = Observer) %>%
  dplyr::rename(Observer.length = Length.analyst) %>%
  dplyr::rename(Comment = Notes) %>%
  dplyr::select(Sample, Latitude, Longitude, Status, Date.time, Site, Location, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment)

names(twenty2)
write.csv(twenty2, "data/raw/ningaloo/2022-05_PtCloates_stereo-BRUVS_Metadata.csv", row.names = FALSE)

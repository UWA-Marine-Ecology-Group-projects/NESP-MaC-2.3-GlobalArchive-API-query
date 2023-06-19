# libraries ---
library(tidyverse)

june20 <- read_csv("data/raw/south-west/2020-06_south-west_stereo-BRUVs_Metadata.csv") %>%
  dplyr::mutate(Date = paste("2020-06", str_sub(Date.time, 1, 2), sep = "-")) %>%
  # dplyr::mutate(Date.time = paste(Date, "T", Time, "+08:00", sep = "")) %>%
  # dplyr::rename(Observer.count = Observer) %>%
  # dplyr::rename(Observer.length = Length.analyst) %>%
  # dplyr::rename(Comment = Notes) %>%
dplyr::select(Sample, Latitude, Longitude, Status, Date.time, Site, Location, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment)
#
unique(june20$Date.time)
#
write.csv(june20, "data/raw/south-west/2020-06_south-west_stereo-BRUVs_Metadata.csv", row.names = FALSE)


oct20 <- read_csv("data/raw/south-west/2020-10_south-west_stereo-BRUVs_Metadata.csv") %>%
  # dplyr::mutate(Date.time1 = with_tz(Date.time, tzone = "Etc/GMT-8")) %>%
  # dplyr::mutate(Date.time = paste(str_replace_all(Date.time1, " ", "T")), "+08:00", sep = "") %>%
  dplyr::mutate(Date = paste("2020-10", str_sub(Date, 7, 8), sep = "-")) %>%
  dplyr::mutate(Date.time = paste(Date, "T", Time, "+08:00", sep = "")) %>%
  dplyr::rename(Observer.count = Observer) %>%
  dplyr::rename(Observer.length = Length.analyst) %>%
  dplyr::rename(Comment = Notes)%>%
  dplyr::filter(Successful.count %in% "Yes") %>%
  dplyr::select(Sample, Latitude, Longitude, Status, Date.time, Site, Location, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment) %>%
  GlobalArchive::ga.clean.names()

#
unique(oct20$Date.time)

write.csv(oct20, "data/raw/south-west/2020-10_south-west_stereo-BRUVs_Metadata.csv", row.names = FALSE)


abrolhos <- read_csv("data/raw/south-west/2021-05_Abrolhos_stereo-BRUVs_Metadata.csv") %>%
  dplyr::mutate(Date = paste("2021-05", str_sub(Date, 7, 8), sep = "-")) %>%
  dplyr::mutate(Date.time = paste(Date, "T", Time, "+08:00", sep = "")) %>%
  dplyr::rename(Observer.count = Observer) %>%
  dplyr::rename(Observer.length = Length.analyst) %>%
  dplyr::rename(Comment = Notes)%>%
  dplyr::filter(Successful.count %in% "Yes") %>%
  dplyr::select(Sample, Latitude, Longitude, Status, Date.time, Site, Location, Depth, Observer.count, Observer.length, Successful.count, Successful.length, Comment) %>%
  GlobalArchive::ga.clean.names()


write.csv(abrolhos, "data/raw/south-west/2021-05_Abrolhos_stereo-BRUVs_Metadata.csv", row.names = FALSE)

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

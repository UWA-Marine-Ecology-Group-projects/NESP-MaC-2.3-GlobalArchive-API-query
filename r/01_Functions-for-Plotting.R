


# Plotting
hab_cols <- scale_fill_manual(values = c("Cnidaria" = "plum",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Seagrasses" = "forestgreen",
                                         "Substrate" = "wheat",
                                         `Sessile invertebrates` = "darkorange",
                                         "Sponges" = "deeppink4"))

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
                                            "Recreational Use Zone" = "#ffb36b",
                                            # "Sanctuary Zone" = "#f7c0d8",
                                            # "Special Purpose Zone" = "#6daff4",
                                            # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                            "Special Purpose Zone (Mining Exclusion)" = "#368ac1"
))

# Shapefile of state marine parks
wampa <- st_read("data/spatial/WA_MPA_2020.shp", crs = 7844) %>%
  st_crop(e) %>%
  st_transform(4326) %>%
  dplyr::mutate(zonename = str_replace_all(ZONE_TYPE, " \\s*\\([^\\)]+\\)", ""))

wampa_fills <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                            # "Marine Nature Reserve" = "#bfd054",
                                            # "Conservation Area" = "#b3a63d",
                                            # "Habitat Protection Zone" = "#fffbcc",# State MPA colours
                                            # "Fish Habitat Protection Area" = "#fbff85",
                                            # "National Park Zone" = "#a4d194",
                                            "General Use" = "#bddde1",
                                            "Recreation Area" = "#f4e952",
                                            "Special Purpose Zone" = "#c5bcc9"
                                            # "Reef Observation Area" = "#ddccff",
                                            # "Marine Management Area" = "#b7cfe1"
))

# Coastal waters limit
cwatr <- st_read("data/spatial/amb_coastal_waters_limit.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

username <- "test"
password <- "gatesttest"

# Define the Indicator species ----
# indicator_species <- c("Chrysophrys auratus",
#                        "Choerodon rubescens",
#                        "Glaucosoma hebraicum",
#                        "Epinephelides armatus",
#                        "Centroberyx gerrardi",
#                        "Nemadactylus valenciennesi",
#                        "Achoerodus gouldii",
#                        "Polyprion oxygeneios",
#                        "Hyporthodus octofasciatus",
#                        "Hyperoglyphe antarctica",
#                        "Lethrinus punctulatus",
#                        "Epinephelus multinotatus",
#                        "Pristipomoides multidens",
#                        "Lethrinus miniatus",
#                        "Lethrinus nebulosus")
indicator_species <- read_csv("data/raw/fish.indicator.species - indicator species.csv") %>%
  dplyr::select(genus, species) %>%
  dplyr::mutate(indicator = "Yes")

lm <- read_csv("data/raw/fish.indicator.species - WA fisheries Lm.csv") %>%
  dplyr::mutate(marine.region = strsplit(as.character(marine.region), split = "/")) %>% # Create a new row for every qualifier - step 1
  unnest(marine.region) %>%
  dplyr::group_by(genus, species) %>% # should add region here if we were doing it properly
  dplyr::summarise(species_lm = max(l50.mm)/10)

# Read in marine parks ----
marineparks <- readRDS("data/spatial/marineparks.RDS")

marineparks$ZONE_TYPE <- str_replace_all(marineparks$ZONE_TYPE, c("[()]" = "",
                                                                  " IUCN IA" = "",
                                                                  " IUCN Ia" = "",
                                                                  " IUCN II" = "",
                                                                  " IUCN IV" = "",
                                                                  " IUCN VI" = "",
                                                                  " IUCN V" = "",
                                                                  "Benthic Protection" = "(Benthic Protection)",
                                                                  "Mining Exclusion" = "(Mining Exclusion)"
                                                                  ))

# Get species list ----
ga.api.species.list <- function() {
  # URL
  url <- paste0("https://gaiastaging.duckdns.org/api/data/GlobalArchiveAustralianFishList/?format=feather")

  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")

    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")

    # Read the Feather file from the input stream
    species_list <- arrow::read_feather(raw_connection) %>%
      as.data.frame() %>%
      left_join(lm) %>%
      dplyr::mutate(fb_length_at_maturity_cm = if_else(is.na(species_lm), fb_length_at_maturity_cm, species_lm))

    names(species_list)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(species_list)

}

# API call for Species Information ----
species_list <- ga.api.species.list() %>%
  dplyr::rename(subject = url)


# Get habitat list ----
ga.api.habitat.list <- function() {
  # URL
  url <- paste0("https://gaiastaging.duckdns.org/api/data/GlobalArchiveBenthicList/?format=feather")

  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")

    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")

    # Read the Feather file from the input stream
    species_list <- arrow::read_feather(raw_connection)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(species_list)

}

# API call for Species Information ----
habitat_list <- ga.api.habitat.list() %>%
  dplyr::rename(subject = url)


# Extract Metadata ----
ga.api.metadata <- function(synthesis_id) {

  # URL
  url <- paste0("https://gaiastaging.duckdns.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&format=feather")

  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")

    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")

    # Read the Feather file from the input stream
    metadata_raw <- arrow::read_feather(raw_connection) %>%
      dplyr::mutate(coordinates = str_replace_all(.$coordinates, c("SRID=4326;POINT " = "", "[()]" = ""))) %>%
      tidyr::separate(coordinates, into = c("longitude", "latitude"), sep = " ") %>%
      dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))%>%
      dplyr::mutate(sample = url) %>%
      dplyr::select(-c(status))

    # Add marine parks to metadata ----
    metadata_spatial <- metadata_raw

    # Add spatial
    coordinates(metadata_spatial) <- c('longitude', 'latitude')
    proj4string(metadata_spatial) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    # Add in marine spatial zoning information ----
    metadata <- bind_cols(metadata_raw, over(metadata_spatial, marineparks)) %>%
      dplyr::rename(zone = ZONE_TYPE) %>%
      tidyr::replace_na(list(status = "Fished")) %>%
      dplyr::select(sample, status, zone, site, location, longitude, latitude, depth, successful_count, successful_length, time_stamp)

    metadata$zone <- fct_relevel(metadata$zone,
                                 "National Park Zone",
                                 "Sanctuary Zone",
                                 "General Use",
                                 "General Use Zone",
                                 "Habitat Protection Zone",
                                 "Multiple Use Zone",
                                 "Recreational Use Zone",
                                 "Special Purpose Zone (Mining Exclusion)")
  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(metadata)

}

# Extract Count ----
ga.api.count <- function(synthesis_id) {
  # URL
  url <- paste0("https://gaiastaging.duckdns.org/api/data/SynthesisCountEntry/?sample__synthesis=", synthesis_id, "&format=feather")

  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")

    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")

    # Read the Feather file from the input stream
    count <- arrow::read_feather(raw_connection) %>%
      mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
      left_join(., species_list, by = "subject") %>%
      left_join(., indicator_species)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(count)

}

# Extract Length ----
ga.api.length <- function(synthesis_id) {
  # URL
  url <- paste0("https://gaiastaging.duckdns.org/api/data/SynthesisLengthEntry/?sample__synthesis=", synthesis_id, "&format=feather")

  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")

    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")

    # Read the Feather file from the input stream
    length <- arrow::read_feather(raw_connection) %>%
      mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
      left_join(., species_list, by = "subject") %>%
      left_join(., indicator_species) %>%
      dplyr::mutate(length_at_maturity = fb_length_at_maturity_cm * 10,
                    scientific = paste(genus, species, sep = " "),
                    community_thermal_index = rls_thermal_niche) %>%
      dplyr::select(sample, caab, family, scientific, genus, species, length, number, range, rms, precision, length_at_maturity, community_thermal_index, indicator)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(length)

}

# Extract Habitat ----
ga.api.habitat <- function(synthesis_id) {
  # URL
  url <- paste0("https://gaiastaging.duckdns.org/api/data/SynthesisBenthosEntry/?sample__synthesis=", synthesis_id, "&format=feather")

  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")

    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")

    # Read the Feather file from the input stream
    habitat <- arrow::read_feather(raw_connection) %>%
      mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveBenthicList")) %>%
      left_join(., habitat_list, by = "subject")

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(habitat)

}

# Extract Habitat Height ----
ga.api.habitat.height <- function(synthesis_id) {
  # URL
  url <- paste0("https://gaiastaging.duckdns.org/api/data/SynthesisBenthosLengthEntry/?sample__synthesis=", synthesis_id, "&format=feather")

  # Send GET request with basic authentication
  response <- GET(url, authenticate(username, password))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Get the raw content
    raw_content <- content(response, "raw")

    # Create an in-memory file-like object from raw content
    raw_connection <- rawConnection(raw_content, "rb")

    # Read the Feather file from the input stream
    habitat_height <- arrow::read_feather(raw_connection) %>%
      mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveBenthicList")) %>%
      left_join(., habitat_list, by = "subject")

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(habitat_height)

}

# Function to create abundance by size of maturity data
ga.five.bins.maturity <- function(complete_data) {

  temp_metadata <- complete_data %>%
    dplyr::distinct(sample, status, zone, site, location, longitude, latitude, depth, successful_count, successful_length, scientific, time_stamp)

  bins <- data.frame(min = c(0,        0.5,      1,         1.25,      1.5),
                     max = c(0.5,      1,        1.25,      1.5,       Inf),
                     label = c("0-50", "50-100", "100-125", "125-150", "150+"))

  binned <- data.frame()

  for(bin in unique(bins$label)) {

    dat <- bins %>%
      dplyr::filter(label %in% bin)

    min <- unique(dat$min)
    max <- unique(dat$max)

    temp_species <- complete_data %>%
      dplyr::filter(length > length_at_maturity * min & length < length_at_maturity * max) %>%
      dplyr::group_by(sample, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::ungroup() %>%
      dplyr::select(sample, scientific, number) %>%
      dplyr::full_join(temp_metadata) %>%
      dplyr::mutate(number = replace_na(number, 0)) %>%
      dplyr::mutate(size.class = bin) %>%
      dplyr::filter(!is.na(scientific))

    temp <- temp_species %>%
      dplyr::group_by(sample) %>%
      dplyr::summarise(number = sum(number)) %>%
      ungroup() %>%
      dplyr::mutate(size.class = bin, scientific = "All indicator species") %>%
      left_join(., temp_metadata %>% dplyr::select(-c(scientific)) %>% distinct()) %>%
      bind_rows(temp_species)

    binned <- bind_rows(binned, temp)

  }

  binned$size.class <- fct_relevel(binned$size.class, c("0-50", "50-100", "100-125", "125-150", "150+"))

  return(binned)

}

# Function to create abundance by size of maturity data
ga.two.bins.maturity <- function(complete_data) {

  temp_metadata <- complete_data %>%
    dplyr::distinct(sample, status, zone, site, location, longitude, latitude, depth, successful_count, successful_length, scientific, time_stamp)

  bins <- data.frame(min = c(0,        1),
                     max = c(1,        Inf),
                     label = c("<Lm", ">Lm"))

  binned <- data.frame()

  for(bin in unique(bins$label)) {

    dat <- bins %>%
      dplyr::filter(label %in% bin)

    min <- unique(dat$min)
    max <- unique(dat$max)

    temp_species <- complete_data %>%
      dplyr::filter(length > length_at_maturity * min & length < length_at_maturity * max) %>%
      dplyr::group_by(sample, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::ungroup() %>%
      dplyr::select(sample, scientific, number) %>%
      dplyr::full_join(temp_metadata) %>%
      dplyr::mutate(number = replace_na(number, 0)) %>%
      dplyr::mutate(size.class = bin) %>%
      dplyr::filter(!is.na(scientific))

    temp <- temp_species %>%
      dplyr::group_by(sample) %>%
      dplyr::summarise(number = sum(number)) %>%
      ungroup() %>%
      dplyr::mutate(size.class = bin, scientific = "All indicator species") %>%
      left_join(., temp_metadata %>% dplyr::select(-c(scientific)) %>% distinct()) %>%
      bind_rows(temp_species)

    binned <- bind_rows(binned, temp)

  }

  binned$size.class <- fct_relevel(binned$size.class, c("<Lm", ">Lm"))

  return(binned)

}


plot_size_of_maturity <- function(length_bins_2, length_bins_5) {

  length_bins_2 <- length_bins_2 %>%
    dplyr::mutate(year = str_sub(time_stamp, 1, 4))

  length_bins_5 <- length_bins_5 %>%
    dplyr::mutate(year = str_sub(time_stamp, 1, 4))

  length_bins_2$size.class <- fct_relevel(length_bins_2$size.class, c("<Lm", ">Lm"))

  length_bins_5$size.class <- fct_relevel(length_bins_5$size.class, c("0-50", "50-100", "100-125", "125-150", "150+"))

  for(species in unique(length_bins_2$scientific)){

    p1 <- ggplot(data = length_bins_2 %>% dplyr::filter(scientific %in% species),
                 aes(x = size.class, y = number)) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
      labs(x = species, y = "Abundance") +
      theme_classic() +
      facet_wrap(~ year)

    p2 <- ggplot(data = length_bins_5 %>% dplyr::filter(scientific %in% species),
                 aes(x = size.class, y = number)) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(alpha = 0.2, position = position_jitter(w = 0.1, h = 0)) +
      labs(x = species, y = "Abundance") +
      theme_classic() +
      facet_wrap(~ year)

    p <- plot_grid(p1, p2, ncol = 1)

    ggsave(filename = paste0("plots/", paste(synthesis, "length.maturity", species, "boxplot.png", sep = "_")),
           plot = p, width = 8, height = 12, units = "in", dpi = 300)

  }
}


username <- "test"
password <- "gatesttest"

# Define the Indicator species ----
indicator_species <- c("Chrysophrys auratus",
                       "Choerodon rubescens",
                       "Glaucosoma hebraicum",
                       "Epinephelides armatus",
                       "Centroberyx gerrardi",
                       "Nemadactylus valenciennesi",
                       "Achoerodus gouldii",
                       "Polyprion oxygeneios",
                       "Hyporthodus octofasciatus",
                       "Hyperoglyphe antarctica",
                       "Lethrinus punctulatus",
                       "Epinephelus multinotatus",
                       "Pristipomoides multidens",
                       "Lethrinus miniatus",
                       "Lethrinus nebulosus")


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
    species_list <- arrow::read_feather(raw_connection)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(species_list)

}

# API call for Species Information ----
species_list <- ga.api.species.list() %>%
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
      tidyr::replace_na(list(status = "Fished"))

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
      left_join(., species_list, by = "subject")

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
    length <- arrow::read_feather(raw_connection)%>%
      mutate(subject = str_replace_all(.$subject, "AnnotationSubject", "GlobalArchiveAustralianFishList")) %>%
      left_join(., species_list, by = "subject")

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(length)

}


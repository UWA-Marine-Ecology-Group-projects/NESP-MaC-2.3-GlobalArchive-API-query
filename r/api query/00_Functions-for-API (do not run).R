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

# # Extract Metadata ----
# ga.api.metadata <- function(synthesis_id) {
#
#   ## Initlialize request with page 1 ----
#   page_number = 1
#
#   response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&page=", page_number),
#                   authenticate("test", "gatesttest"),
#                   # add_headers(Authorization = "Bearer your_access_token"),
#                   content_type("application/json"))
#
#   json_data <- content(response, "text", encoding = "UTF-8")
#   parsed_data <- jsonlite::fromJSON(json_data)
#
#   next_page <- length(parsed_data$`next`)
#
#   metadata <- parsed_data$results
#
#   message(paste("Connection successful. Downloading metadata for synthesis:", name, sep = " "))
#
#   # Need to stop when next is NULL (parsed_data$next)
#   while (!next_page == 0) {
#     page_number <- page_number + 1 # Increase page number
#
#     # message(paste0("Up to page ", page_number))
#
#     new_response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&page=", page_number),
#                         authenticate("test", "gatesttest"),
#                         # add_headers(Authorization = "Bearer your_access_token"),
#                         content_type("application/json"))
#
#     new_json_data <- content(new_response, "text", encoding = "UTF-8")
#     new_parsed_data <- jsonlite::fromJSON(new_json_data)
#
#     new_metadata <- new_parsed_data$results
#
#     metadata <- bind_rows(metadata, new_metadata)
#
#     next_page <- length(new_parsed_data$`next`)
#   }
#
#   message(paste("Finished downloading metadata for synthesis:", name, sep = " "))
#
#   return(metadata)
#
# }
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
    metadata <- arrow::read_feather(raw_connection)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(metadata)

}

# Extract Count ----
# ga.api.count <- function(synthesis_id) {
#   ## Initlialize request with page 1 ----
#   page_number = 1
#   response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisCountEntry/?sample__synthesis=", synthesis_id, "&page=", page_number),
#                   authenticate("test", "gatesttest"),
#                   # add_headers(Authorization = "Bearer your_access_token"),
#                   content_type("application/json"))
#
#   json_data <- content(response, "text", encoding = "UTF-8")
#   parsed_data <- jsonlite::fromJSON(json_data)
#
#   next_page <- length(parsed_data$`next`)
#
#   count <- parsed_data$results
#
#   message(paste("Connection successful. Downloading count for synthesis:", name, "(Please note this may take a while to run for large syntheses)", sep = " "))
#
#   while (!next_page == 0) {
#     page_number <- page_number + 1 # Increase page number
#
#     # message(page_number)
#
#     new_response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisCountEntry/?sample__synthesis=", synthesis_id, "&page=", page_number),
#                         authenticate("test", "gatesttest"),
#                         # add_headers(Authorization = "Bearer your_access_token"),
#                         content_type("application/json"))
#
#     new_json_data <- content(new_response, "text", encoding = "UTF-8")
#     new_parsed_data <- jsonlite::fromJSON(new_json_data)
#
#     new_count <- new_parsed_data$results
#
#     count <- bind_rows(count, new_count)
#
#     next_page <- length(new_parsed_data$`next`)
#   }
#
#   message(paste("Finished downloading count for synthesis:", name, sep = " "))
#
#   return(count)
#
# }
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
    count <- arrow::read_feather(raw_connection)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(count)

}

# Extract Length ----
# ga.api.length <- function(synthesis_id) {
#
#   ## Initlialize request with page 1 ----
#   page_number = 1
#
#   response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisLengthEntry/?sample__synthesis=", synthesis_id,"&page=", page_number),
#                   authenticate("test", "gatesttest"),
#                   # add_headers(Authorization = "Bearer your_access_token"),
#                   content_type("application/json"))
#
#   json_data <- content(response, "text", encoding = "UTF-8")
#   parsed_data <- jsonlite::fromJSON(json_data)
#
#   next_page <- length(parsed_data$`next`)
#
#   length <- parsed_data$results
#
#   message(paste("Connection successful. Downloading length for synthesis:", name, "(Please note this may take a while to run for large syntheses)", sep = " "))
#
#   while (!next_page == 0) {
#     page_number <- page_number + 1 # Increase page number
#     # message(page_number)
#
#     new_response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisLengthEntry/?sample__synthesis=", synthesis_id,"&page=", page_number),
#                         authenticate("test", "gatesttest"),
#                         # add_headers(Authorization = "Bearer your_access_token"),
#                         content_type("application/json"))
#
#     new_json_data <- content(new_response, "text", encoding = "UTF-8")
#     new_parsed_data <- jsonlite::fromJSON(new_json_data)
#
#     new_length <- new_parsed_data$results
#
#     length <- bind_rows(length, new_length)
#
#     next_page <- length(new_parsed_data$`next`)
#   }
#
#   message(paste("Finished downloading length for synthesis:", name, sep = " "))
#
#   return(length)
# }

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
    length <- arrow::read_feather(raw_connection)

  } else {
    # Request was not successful
    cat("Request failed with status code:", status_code(response))
  }

  return(length)

}

# ga.api.species.list <- function(data){
#   # Extract species information ----
#   unique.species <- unique(count$subject)
#
#   species_list <- data.frame()
#
#   message(paste("Connection successful. Downloading species list for synthesis:", name, "(Please note this may take a while to run for large syntheses)", sep = " "))
#
#   for(species in seq(1:length(unique.species))){
#
#     # message(species)
#
#     response <- GET(paste0(unique.species[species]),
#                     authenticate("test", "gatesttest"),
#                     # add_headers(Authorization = "Bearer your_access_token"),
#                     content_type("application/json"))
#
#     json_data <- content(response, "text", encoding = "UTF-8")
#     temp_species <- fromJSON(json_data, preserveNaN = TRUE)
#     species_list <- bind_rows(species_list, temp_species)
#
#   }
#
#   species_list <- species_list %>%
#     dplyr::rename(subject = url) %>%
#     distinct()
#
#   message(paste("Finished downloading species list for synthesis:", name, sep = " "))
#
#   return(species_list)
# }

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

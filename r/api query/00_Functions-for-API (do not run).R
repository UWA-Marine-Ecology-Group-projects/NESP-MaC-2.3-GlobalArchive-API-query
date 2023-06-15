
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


# Extract Metadata ----
ga.api.metadata <- function(synthesis_id) {

  ## Initlialize request with page 1 ----
  page_number = 1

  response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&page=", page_number),
                  authenticate("test", "gatesttest"),
                  # add_headers(Authorization = "Bearer your_access_token"),
                  content_type("application/json"))

  json_data <- content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(json_data)

  next_page <- length(parsed_data$`next`)

  metadata <- parsed_data$results

  # Need to stop when next is NULL (parsed_data$next)
  while (!next_page == 0) {
    page_number <- page_number + 1 # Increase page number

    message(paste0("Up to page ", page_number))

    new_response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisSample/?synthesis=", synthesis_id, "&page=", page_number),
                        authenticate("test", "gatesttest"),
                        # add_headers(Authorization = "Bearer your_access_token"),
                        content_type("application/json"))

    new_json_data <- content(new_response, "text", encoding = "UTF-8")
    new_parsed_data <- jsonlite::fromJSON(new_json_data)

    new_metadata <- new_parsed_data$results

    metadata <- bind_rows(metadata, new_metadata)

    next_page <- length(new_parsed_data$`next`)
  }

  message("Finished downloading")

  return(metadata)

}

# Extract Count ----
ga.api.count <- function(synthesis_id) {
  ## Initlialize request with page 1 ----
  page_number = 1
  response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisCountEntry/?sample__synthesis=", synthesis_id, "&page=", page_number),
                  authenticate("test", "gatesttest"),
                  # add_headers(Authorization = "Bearer your_access_token"),
                  content_type("application/json"))

  json_data <- content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(json_data)

  next_page <- length(parsed_data$`next`)

  count <- parsed_data$results

  message("Connection successful. Please note this may take a while to run for large syntheses")

  while (!next_page == 0) {
    page_number <- page_number + 1 # Increase page number

    # message(page_number)

    new_response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisCountEntry/?sample__synthesis=", synthesis_id, "&page=", page_number),
                        authenticate("test", "gatesttest"),
                        # add_headers(Authorization = "Bearer your_access_token"),
                        content_type("application/json"))

    new_json_data <- content(new_response, "text", encoding = "UTF-8")
    new_parsed_data <- jsonlite::fromJSON(new_json_data)

    new_count <- new_parsed_data$results

    count <- bind_rows(count, new_count)

    next_page <- length(new_parsed_data$`next`)
  }

  message("Finished downloading")

  return(count)

}

# Extract Length ----
ga.api.length <- function(synthesis_id) {

  ## Initlialize request with page 1 ----
  page_number = 1

  response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisLengthEntry/?sample__synthesis=", synthesis_id,"&page=", page_number),
                  authenticate("test", "gatesttest"),
                  # add_headers(Authorization = "Bearer your_access_token"),
                  content_type("application/json"))

  json_data <- content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(json_data)

  next_page <- length(parsed_data$`next`)

  length <- parsed_data$results

  message("Connection successful. Please note this may take a while to run for large syntheses")

  while (!next_page == 0) {
    page_number <- page_number + 1 # Increase page number
    # message(page_number)

    new_response <- GET(paste0("https://gaiastaging.duckdns.org/api/data/SynthesisLengthEntry/?sample__synthesis=", synthesis_id,"&page=", page_number),
                        authenticate("test", "gatesttest"),
                        # add_headers(Authorization = "Bearer your_access_token"),
                        content_type("application/json"))

    new_json_data <- content(new_response, "text", encoding = "UTF-8")
    new_parsed_data <- jsonlite::fromJSON(new_json_data)

    new_length <- new_parsed_data$results

    length <- bind_rows(length, new_length)

    next_page <- length(new_parsed_data$`next`)
  }

  message("Finished downloading")

  return(length)
}

ga.api.species.list <- function(data){
  # Extract species information ----
  unique.species <- unique(count$subject)

  species_list <- data.frame()

  message("Please note this may take a while to run for large syntheses")

  for(species in seq(1:length(unique.species))){

    # message(species)

    response <- GET(paste0(unique.species[species]),
                    authenticate("test", "gatesttest"),
                    # add_headers(Authorization = "Bearer your_access_token"),
                    content_type("application/json"))

    json_data <- content(response, "text", encoding = "UTF-8")
    temp_species <- fromJSON(json_data, preserveNaN = TRUE)
    species_list <- bind_rows(species_list, temp_species)

  }

  species_list <- species_list %>%
    dplyr::rename(subject = url) %>%
    distinct()

  message("Finished downloading")

  return(species_list)
}

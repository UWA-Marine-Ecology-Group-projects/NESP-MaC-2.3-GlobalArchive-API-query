library(shinydashboard)
library(leaflet)
library(dplyr)
library(leafsync)

function(input, output, session) {

  data <- reactive({

    if(input$dataset %in% "Geographe Bay") {
      data <- readRDS("output/Geographe-bay_length-class.RDS")
    } else {
      data <- readRDS("output/Ningaloo_length-class.RDS")
    }

    data

  })

  # Create an UI output for fish ----
  output$fish <- renderUI({

    options <- data() %>%
      distinct(scientific) %>%
      pull("scientific")

    selectInput(
      inputId = "fish",
      label = "Choose a species:",
      choices = c(options),
      multiple = FALSE,
      selectize = TRUE)

  })

  output$leaflet <- renderUI({

    dat.small <- data() %>%
      dplyr::filter(scientific %in% input$fish) %>%
      dplyr::filter(class %in% "< 100%")

    leaf.small <- leaflet(dat.small) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~(count),
        stroke = FALSE, fillOpacity = 0.5)

    dat.big <- data() %>%
      dplyr::filter(scientific %in% input$fish) %>%
      dplyr::filter(!class %in% "< 100%") %>%
      glimpse()

    leaf.big <- leaflet(dat.big) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~(count),
        stroke = FALSE, fillOpacity = 0.5)

    leaflets <- leafsync::sync(leaf.small, leaf.big)

    leaflets


  })

}

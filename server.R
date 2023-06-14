library(shinydashboard)
library(leaflet)
library(dplyr)

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

  output$leaflet <- renderLeaflet({

    dat <- data() %>%
      dplyr::filter(scientific %in% input$fish)

    leaflet(dat) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~(count),
        stroke = FALSE, fillOpacity = 0.5
      )

  })

}

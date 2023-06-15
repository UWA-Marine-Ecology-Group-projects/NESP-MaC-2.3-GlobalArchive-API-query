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

    overzero <- filter(dat.small, count > 0)
    equalzero <- filter(dat.small, count ==  0)

    leaf.small <- leaflet(dat.small) %>%
      addTiles()

    if (nrow(overzero)) {
      leaf.small <- leaf.small %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((count/max(count))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(count)
        )
    }
    if (nrow(equalzero)) {
      leaf.small <- leaf.small %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
          label = ~as.character(count)
        )
    }

    dat.big <- data() %>%
      dplyr::filter(scientific %in% input$fish) %>%
      dplyr::filter(!class %in% "< 100%")

    overzero <- filter(dat.big, count > 0)
    equalzero <- filter(dat.big, count ==  0)

    leaf.big <- leaflet(dat.big) %>%
      addTiles()

    if (nrow(overzero)) {
      leaf.big <- leaf.big %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((count/max(count))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(count)
        )
    }
    if (nrow(equalzero)) {
      leaf.big <- leaf.big %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
          label = ~as.character(count)
        )
    }

    leaflets <- leafsync::sync(leaf.small, leaf.big)

    leaflets


  })

}

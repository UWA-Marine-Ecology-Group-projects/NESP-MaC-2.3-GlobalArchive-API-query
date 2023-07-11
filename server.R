library(shinydashboard)
library(leaflet)
library(dplyr)
library(leafsync)
library(stringr)
library(ggplot2)
library(leafgl)

se <- function(x) {
  sd(x) / sqrt(length(x))
}


se.min <- function(x) {
  (mean(x)) - se(x)
}

se.max <- function(x) {
  (mean(x)) + se(x)
}

geo.pal <- c("National Park Zone" = "#7BBC63",
             "Sanctuary Zone" = "#206108", # Made darker so it doesn't clash with Commonwealth
             "General Use Zone" = "#bd405f", # Made darker so it doesn't clash with Commonwealth
             "Habitat Protection Zone" = "#FFF8A3",
             "Special Purpose Zone (Mining Exclusion)" = "#6CAFDF",
             "Multiple Use Zone" = "#B8E5FA")

ning.pal <- c("National Park Zone" = "#7BBC63",
              "Sanctuary Zone" = "#206108", # Made darker so it doesn't clash with Commonwealth
              "General Use" = "#bd405f", # Made darker so it doesn't clash with Commonwealth
              "Multiple Use Zone" = "#B8E5FA",
              "Recreational Use Zone" = "#FDB933",
              "Special Purpose Zone (Benthic Protection)" = "#c76bc4"
              )


ggplot_mpatheme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme( # use theme_get() to see available options
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_blank(),
      # legend.position = "top",
      text = ggplot2::element_text(size = 12),
      strip.text.y = ggplot2::element_text(size = 12, angle = 0),
      axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
      axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
      axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
    )
}

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


  output$temporal <- renderPlot({

    dat <- data() %>%
      dplyr::filter(scientific %in% "All indicator species") %>%
      # dplyr::filter(class %in% "< 100%") %>%
      dplyr::mutate(year = as.numeric(str_sub(time_stamp, 1, 4))) %>%
      glimpse()

    glimpse(names(dat))

    if(input$dataset %in% "Ningaloo"){
      pal <- ning.pal
    } else {
      pal <- geo.pal
    }

    ggplot(dat, aes(x = year, y = count, fill = zone)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance per sample \n(+/- SE)") +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
        expand = expand_scale(mult = c(0.25, 0.25))
      ) +
      ggplot_mpatheme() +
      geom_vline(xintercept = 2018, linetype = 3) +
      scale_fill_manual(values = c(pal)) +
      facet_wrap(class ~ ., scales = "free", ncol = 2)
  })

  output$temporal.status <- renderPlot({

    dat <- data() %>%
      dplyr::filter(scientific %in% "All indicator species") %>%
      dplyr::mutate(year = as.numeric(str_sub(time_stamp, 1, 4))) %>%
      glimpse()

    glimpse(names(dat))

    ggplot(dat, aes(x = year, y = count, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance per sample \n(+/- SE)") +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
        expand = expand_scale(mult = c(0.25, 0.25))
      ) +
      ggplot_mpatheme() +
      geom_vline(xintercept = 2018, linetype = 3) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb",
                                   "No-take" = "#7bbc63")) +
      facet_wrap(class ~ ., scales = "free", ncol = 2)
  })
}

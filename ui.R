library(shinydashboard)
library(leaflet)
library(dplyr)

dashboardPage(
  dashboardHeader(title = "Workshop Demo"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fish", tabName = "fish", icon = icon("fish")),
      menuItem("Habitat", tabName = "habitat", icon = icon("tree"))
    )
  ),

  ## Body content
  dashboardBody(
    tabItems(
      # Fish tab content
      tabItem(tabName = "fish",
              fluidRow(
                box(title = "Select data", width = 12,
                  selectInput(
                    inputId = "dataset",
                    label = "Choose a synthesis data-set:",
                    choices = c("Ningaloo", "Geographe Bay"),
                    selected = "Ningaloo",
                    multiple = FALSE,
                    selectize = TRUE),

                  uiOutput("fish")

                ),

                tableOutput("data"),

                box(
                  title = "Interactive spatial plot",
                  width = 12,
                  leafletOutput("leaflet", height = "500px")
                )
              )
      ),

      # Second tab content
      tabItem(tabName = "habitat",
              h2("Habitat tab content to be added...")
      )
    )
  )
)

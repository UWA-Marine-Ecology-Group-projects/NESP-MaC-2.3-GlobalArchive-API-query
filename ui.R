library(shinydashboard)
library(leaflet)
library(dplyr)
library(leafsync)
library(stringr)

dashboardPage(
  dashboardHeader(title = "Workshop Demo"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fish", tabName = "fish", icon = icon("fish"))#,
      # menuItem("Habitat", tabName = "habitat", icon = icon("tree"))
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
                  title = "Interactive spatial plots",
                  width = 12,
                  column(width = 6, align="center", h3("Smaller than length at Maturity")),
                  column(width = 6, align="center", h3("Larger than length at Maturity")),
                  uiOutput("leaflet", height = "500px")
                ),


                box(
                  title = "Temporal plot by Status - All indicator species",
                  width = 12,
                  # column(width = 12, plotOutput("temporal")),
                  column(width = 12, plotOutput("temporal.status"))
                ),

                box(
                  title = "Temporal plot by Zone - All indicator species",
                  width = 12,
                  column(width = 12, plotOutput("temporal"))#,
                  # column(width = 12, plotOutput("temporal.status"))
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

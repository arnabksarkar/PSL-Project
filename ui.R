## ui.R

## if (!("ShinyRatingInput" %in% installed.packages())) {
##    install.packages("devtools")
##    devtools::install_github("stefanwilhelm/ShinyRatingInput")
## }

library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(shinyjs)
library(ShinyRatingInput)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          dashboardSidebar(
            sidebarMenu(
              id = "tabs",
              menuItem("Movies by Genre", tabName = "genre"),
              menuItem("Movies By User Rating", tabName = "rating")
            )
          ),
          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                tabItem(
                  tabName = "genre",
                  fluidRow(
                    box(width = 12, title = "Select Your Favorite Genre", status = "info", solidHeader = TRUE, collapsible = FALSE,
                        uiOutput('genres_picklist')
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Top Movies",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btnGenre", "Click to get top Genre movies", class = "btn-info")
                      ),
                      br(),
                      tableOutput("results_by_genre")
                    )
                  )
                ),
                tabItem(
                  tabName = "rating",
                  fluidRow(
                    box(width = 12, title = "Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings_movie')
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btnRating", "Click here to get your recommendations", class = "btn-info")
                      ),
                      br(),
                      tableOutput("results")
                    )
                  )
                )
              )
          )
    )
) 
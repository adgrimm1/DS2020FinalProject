#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(tigris)

# Load or simulate your dataset
# Assume data_frame includes columns: Year, StateAbbr, CountyName, Measure, County_State_Avg_Diff, County_National_Avg_Diff
State_County_Map <- data_frame %>%
  select(Year, StateAbbr, CountyName, Measure, County_State_Avg_Diff, County_National_Avg_Diff) %>%
  filter(!is.na(CountyName))

# Load U.S. county shapefile data
options(tigris_use_cache = TRUE)
us_counties <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive County-Level Health Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique(State_County_Map$StateAbbr)),
      selectInput("measure", "Select Measure:", choices = unique(State_County_Map$Measure)),
      radioButtons("comparison", "Compare Against:",
                   choices = c("National Average" = "County_National_Avg_Diff",
                               "State Average" = "County_State_Avg_Diff"),
                   selected = "County_State_Avg_Diff")
    ),
    mainPanel(
      leafletOutput("countyMap")
    )
  )
)

# Define Server
server <- function(input, output) {

  # Filter data based on user input
  filtered_data <- reactive({
    State_County_Map %>%
      filter(StateAbbr == input$state, Measure == input$measure)
  })

  # Merge with shapefile
  county_map_data <- reactive({
    state_counties <- us_counties %>%
      filter(STUSPS == input$state) # Filter counties for selected state

    # Merge geographic data with filtered data
    state_counties <- state_counties %>%
      left_join(filtered_data(), by = c("NAME" = "CountyName"))

    state_counties
  })

  # Render Leaflet Map
  output$countyMap <- renderLeaflet({
    map_data <- county_map_data()

    # Create color palette
    pal <- colorNumeric(palette = "RdBu", domain = map_data[[input$comparison]], na.color = "gray80")

    # Generate Leaflet map
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(get(input$comparison)),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE),
        label = ~paste0(NAME, ": ", round(get(input$comparison), 2))
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = map_data[[input$comparison]],
        title = paste("Comparison:", ifelse(input$comparison == "County_State_Avg_Diff", "State Average", "National Average")),
        opacity = 1
      )
  })
}

# Run the App
shinyApp(ui = ui, server = server)

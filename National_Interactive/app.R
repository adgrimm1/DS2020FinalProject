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

# UI
ui <- fluidPage(
  titlePanel("Interactive State Ranking Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("measure_id", "Select Measure ID:",
                  choices = unique(map_data_states$MeasureId))
    ),
    mainPanel(
      plotOutput("state_map")
    )
  )
)

# Server
server <- function(input, output) {

  output$state_map <- renderPlot({
    # Filter data based on selected MeasureId
    filtered_data <- map_data_states %>% filter(MeasureId == input$measure_id)

    # Generate the map
    ggplot(filtered_data, aes(x = long, y = lat, group = group, fill = State_National_Avg_Diff)) +
      geom_polygon(color = "white", size = 0.2) +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red", midpoint = 0,
        limits = c(-15, 15), oob = scales::squish
      ) +
      coord_fixed(1.3) +
      labs(
        title = paste("State National Average Difference for", input$measure_id),
        fill = "Avg Diff"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

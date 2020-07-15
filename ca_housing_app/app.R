library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)

housing <- read_csv("housing.csv")

ocean_prox <- unique(housing$ocean_proximity)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # App theme
    bootstrapPage(
    theme = shinytheme("united"),
    
    navbarPage("California Housing",
               tabPanel("Map",
                        
                        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                        plotOutput("leafletPlot", width = "100%", height = "100%"),
                        absolutePanel(#top = 10, right = 10,
                                      sliderInput("value",
                                                  "House Value Range",
                                                  min = min(housing$median_house_value),
                                                  max = max(housing$median_house_value),
                                                  value = c(min, max)
                                      ),
                                      
                                      checkboxGroupInput("location", 
                                                         "Location", 
                                                         choices = list("Near Bay" = "NEAR BAY", 
                                                                        "<1hr to Ocean" = "<1H OCEAN", 
                                                                        "Inland" = "INLAND", 
                                                                        "Near Ocean" = "NEAR OCEAN", 
                                                                        "Island" = "ISLAND"),
                                                         selected = c("NEAR BAY", 
                                                                      "<1H OCEAN", 
                                                                      "INLAND", 
                                                                      "NEAR OCEAN", 
                                                                      "ISLAND")
                                                         
                                      ))))))
    




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$leafletPlot <- renderLeaflet({
        
        # Create a color palette with handmade bins.
        mybins <- seq(min(housing$median_house_value), max(housing$median_house_value),
                      length.out = 7
        )
        
        mypalette <- colorBin(
            palette = "magma",
            domain = housing$median_house_value,
            na.color = "transparent",
            bins = mybins
        )
        
        housing %>% 
            filter(
                median_house_value >= input$value[1],
                median_house_value <= input$value[2],
                ocean_proximity %in% input$location) %>% 
            leaflet() %>%
            addProviderTiles(
                providers$CartoDB.Positron
            ) %>%
            addCircles(
                lat = ~latitude,
                lng = ~longitude,
                color = ~ mypalette(median_house_value),
                fillOpacity = 0.5,
                #radius = ~population,
                stroke = FALSE
            ) %>%
            addLegend(
                pal = mypalette,
                values = ~median_house_value,
                opacity = 0.9,
                title = "Median House Value $",
                position = "topright"
                # Can't make labels work when using pal instead of col
                # labels = c("< $100,000", 
                #            "< $200,000", 
                #            "< $300,000", 
                #            "< $400,000", 
                #            "< $500,000", 
                #            "< $500,001")
                
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

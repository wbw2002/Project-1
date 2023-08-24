library("lubridate")
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)
library(rsconnect)
library(maps)

# UPDATED TABLE CODE
earthquake_data <- read.csv('https://raw.githubusercontent.com/info-201b-sp23/exploratory-analysis-Ncumic/main/earthquake_data.csv')
earthquake_data_modified <- select(earthquake_data, -c("title","net", "nst", "dmin", "gap", "magType", "depth"))

# ADD SUMMARY VALUES HERE
source("Summary.R")



server <- function(input, output) {
  # NIKOLA SECTION

  world_data_shape <- map_data("world")


  output$Nikola_Plot <- renderPlotly({


    earthquake_data_modified <- earthquake_data_modified %>% filter(magnitude >= input$Variables[1] & magnitude <= input$Variables[2])


    Earthquake_plot <- ggplot(data = world_data_shape) +
      geom_polygon(aes(x = long,
                       y = lat,
                       group = group)) +
      geom_point(data = earthquake_data_modified,
                 aes(x = longitude, y = latitude,
                     size = magnitude),
                 color = "Red",
                 shape = 21
      ) + labs(title = "Earthquake locations & respective magnitudes",
               x = "Longitude",
               y = "Latitude"
    )
    return(Earthquake_plot)
  })

  
  #Bonie SECTION
  
  output$BonieChart <- renderPlotly({
    filtered_data <- CountryAccuracy %>% 
      filter(country %in% c(input$country1, input$country2))
    
    plot <- ggplot(filtered_data, aes(x = Year, y = mean_accuracy, color = country)) +
      # geom_line() +
      geom_smooth(se=FALSE) +
      labs(x = "Year", y = "Mean Accuracy", title = "Comparison of Estimation Accuracy of different countries") +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly::ggplotly(plot)
  })

  # BRIAN SECTION
  eq_df <- read.csv("https://raw.githubusercontent.com/info-201b-sp23/exploratory-analysis-Ncumic/main/earthquake_data.csv",
                    stringsAsFactors = FALSE)
  output$plot <- renderPlotly({
    world_shape <- map_data("world")

    filtered_eq_df <- filter(eq_df, country == input$country)

    eq_mapping_df <- left_join(world_shape, filtered_eq_df, by = c("long" = "longitude", "lat" = "latitude"))

    world_plot <- ggplot(data = world_shape) +
      geom_polygon(aes(x = long, y = lat, group = group)) +
      geom_point(data = filter(filtered_eq_df, tsunami == 0), aes(x = longitude, y = latitude, color = "Non-Tsunami"), size = 2) +
      geom_point(data = filter(filtered_eq_df, tsunami == 1), aes(x = longitude, y = latitude, color = "Tsunami"), size = 2) +
      labs(title = paste("Earthquakes That Caused Tsunamis in", input$country), x = "Longitude", y = "Latitude") +
      scale_color_manual(values = c("Non-Tsunami" = "blue", "Tsunami" = "red"),
                         labels = c("Non-Tsunami", "Tsunami"),
                         name = "Tsunami")

    return (world_plot)
  })


  


}
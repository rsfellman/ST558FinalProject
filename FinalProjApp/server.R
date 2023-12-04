# Shiny Server for Final Project
# Rachel Fellman



# load necessary packages

library(shiny)
library(tidyverse)
library(maps)

# read in data with a relative path
landslide<- read_csv("Global_Landslide_Catalog_Export.csv")

#drop missing fatality count observations
land.na <- landslide %>% drop_na(fatality_count)

#create and order factor for landslide size variable
land.na$landslide_size <- as.factor(land.na$landslide_size)
land.na$landslide_size <- factor(land.na$landslide_size, levels = c("catastrophic", "very_large", "large", "medium", "small", "unknown"))

#filter US data with filter function
land.us<- land.na %>% 
  filter(country_code == "US")



# Define server logic 
function(input, output, session) {

  # include landslide image with renderImage function
  output$image <- renderImage({
    list(src = "landslide.png")
  }, deleteFile = FALSE
  )
  
  #create data exploration plots using conditional logic that changes the plot if the user selects a different type of summary from the drop down menu
  output$sum.plot <- renderPlot({
    #create base plot
    g<- ggplot()
    #get world map data
    world<- map_data("world")
    #get us map data
    states<- map_data("state")
    
    
    
    #create graph with condition world map colored by fatalities
    if (input$graphsum == 1 & input$color1 == 1) {
      #create world map plot
      g +  geom_polygon(data = world, aes(x=long, y= lat, group = group), color = "darkgrey", fill = "grey") +
          #add the points of landslides
        geom_point(data = landslide, aes(x = longitude, y = latitude, colour = landslide_size) ) +
        #add a color gradient for the size of landslide
        scale_colour_manual(name = "Landslide Size", values = c("catastrophic" = "darkred", "very_large" = "red", "large" = "orange", "medium" = "yellow", "small" = "green", "unknown"= "darkgrey"))+
        #add labels.
        labs(x = "Longitude", y = "Latitude", title = "World Map of Landslides")
      
      #create graph with condition world map colored by injuries
    } else if (input$graphsum == 1 & input$color1 == 2) {
      #create world map plot
      g +  geom_polygon(data = world, aes(x=long, y= lat, group = group), color = "darkgrey", fill = "grey") +
        #add the points of landslides
        geom_point(data = landslide, aes(x = longitude, y = latitude, colour = injury_count) ) +
        #add a color gradient for the injuries
        scale_colour_gradient(low = "green", high = "red", name = "Injuries")+
        #add labels.
        labs(x = "Longitude", y = "Latitude", title = "World Map of Landslides")
      
      #create graph with condition US map and fatalities.
    } else if (input$graphsum == 2 & input$color2 == 1) {
      #create world map plot
      g +  geom_polygon(data = states, aes(x=long, y= lat, group = group), color = "darkgrey", fill = "grey") +
        #add the points of landslides
        geom_point(data = land.us, aes(x = longitude, y = latitude, colour = fatality_count) ) +
        #add a color gradient for the fatalities
        scale_colour_gradient(low = "lightblue", high = "darkblue", name = "Fatalities")+
        #add labels.
        #adjust the coordinates
        coord_cartesian(xlim = c(-127,-65), ylim = c(25,50)) +
        #add labels.
        labs(x = "Longitude", y = "Latitude", title = "United States Map of Landslides")
      
      #create graph for conditions us map and population
    } else if (input$graphsum ==2 & input$color2 ==2) {
      g +  geom_polygon(data = states, aes(x=long, y= lat, group = group), color = "darkgrey", fill = "grey") +
        #add the points of landslides
        geom_point(data = land.us, aes(x = longitude, y = latitude, colour = landslide_size) ) +
        #add a color gradient for the size of landslide
        scale_colour_manual(name = "Landslide Size", values = c("catastrophic" = "darkred", "very_large" = "red", "large" = "orange", "medium" = "yellow", "small" = "green", "unknown"= "darkgrey"))+
        #adjust the coordinates
        coord_cartesian(xlim = c(-127,-65), ylim = c(25,50)) +
        #add labels.
        labs(x = "Longitude", y = "Latitude", title = "United States Map of Landslides")
      
      #create graph for the conditions bar graph and setting
    } else if (input$graphsum ==3 & input$color3 ==1) {
      #create bar plot with user input for the fill
     g + geom_bar(data = land.na, aes(x = landslide_size, fill = as.factor(landslide_setting))) +
      #add labels
      scale_fill_discrete(name = "Setting") +
      labs(title = "Bar Graph of Landslide Size")
    }

    })
  
  
  
}

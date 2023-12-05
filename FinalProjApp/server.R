# Shiny Server for Final Project
# Rachel Fellman



# load necessary packages

library(shiny)
library(tidyverse)
library(maps)
library(DT)
library(rlang)
library(mathjaxr)

# read in data with a relative path
landslide<- read_csv("Global_Landslide_Catalog_Export.csv")

#drop missing fatality count observations
land.na <- landslide %>% drop_na(fatality_count)

#create and order factor for landslide size variable
land.na$landslide_size <- as.factor(land.na$landslide_size)
#reorder landslide size
land.na$landslide_size <- factor(land.na$landslide_size, levels = c("catastrophic", "very_large", "large", "medium", "small", "unknown"))
#make country name a factor
land.na$country_name <- as.factor(land.na$country_name)
#make landslide category a factor
land.na$landslide_category <-as.factor(land.na$landslide_category)
#make landslide trigger a factor
land.na$landslide_trigger <-as.factor(land.na$landslide_trigger)
#make landslide setting a factor
land.na$landslide_setting <-as.factor(land.na$landslide_setting)

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
    #remove fatality outlier
    land.out <- land.na %>% 
      filter(fatality_count < 1000)
    
    
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
      
      
      #create graph for the conditions bar graph and landslide setting
    } else if (input$graphsum ==3 & input$color3 ==1) {
      #create bar plot fill by landslide setting
     g + geom_bar(data = land.na, aes(x = landslide_size, fill = as.factor(landslide_setting))) +
      #add labels to legend
      scale_fill_discrete(name = "Setting") +
        #add title
      labs(title = "Bar Graph of Landslide Size")
      
      #create graph for the conditions bar graph and landslide trigger
    } else if (input$graphsum ==3 & input$color3 ==2) {
      #create bar plot with fill by landslide trigger
      g + geom_bar(data = land.na, aes(x = landslide_size, fill = as.factor(landslide_trigger)))+
        #add labels to legend
        scale_fill_discrete(name = "Trigger") +
        #add title
        labs(title = "Bar Graph of Landslide Size")
      
      #create graph for the conditions bar graph and landslide category
    } else if (input$graphsum ==3 & input$color3 == 3) {
      #create bar plot with fill by landslide category
      g + geom_bar(data = land.na, aes(x = landslide_size, fill = as.factor(landslide_category)))+
        #add labels to legend
        scale_fill_discrete(name = "Category") +
        #add title
        labs(title = "Bar Graph of Landslide Size")
      
      
      #create graph for conditions scatterplot and no boxes checked
    } else if (input$graphsum == 4 & input$outlier == FALSE & input$color4 == FALSE) {
      #create scatter plot
      g + geom_jitter(data = land.na, aes(x = fatality_count, y = admin_division_population)) +
      #add labels
      labs(title = "Scatter Plot of Landslide Fatality Count by Population", x = "Fatality Count", y = "Population" )
      
      #create graph for conditions scatterplot and outlier box checked
    } else if (input$graphsum == 4 & input$outlier == TRUE & input$color4 == FALSE) {
      g + geom_jitter(data = land.out, aes(x = fatality_count, y = admin_division_population)) +
        #add labels
        labs(title = "Scatter Plot of Landslide Fatality Count by Population", x = "Fatality Count", y = "Population" )
      
      #create graph for conditions scatter plot and outlier and size box checked
    } else if (input$graphsum == 4 & input$outlier == TRUE & input$color4 == TRUE) {
      g + geom_jitter(data = land.out, aes(x = fatality_count, y = admin_division_population, color = landslide_size)) +
        #add labels
        labs(title = "Scatter Plot of Landslide Fatality Count by Population", x = "Fatality Count", y = "Population" ) +
      #adjust the legend
    scale_colour_manual(name = "Landslide Size", values = c("catastrophic" = "darkred", "very_large" = "red", "large" = "orange", "medium" = "yellow", "small" = "green", "unknown"= "darkgrey"))
      
      #create graph for conditions scatter plot and size box checked
    } else if (input$graphsum == 4 & input$outlier == FALSE & input$color4 == TRUE) {
      g + geom_jitter(data = land.na, aes(x = fatality_count, y = admin_division_population, color = landslide_size)) +
        #add labels
        labs(title = "Scatter Plot of Landslide Fatality Count by Population", x = "Fatality Count", y = "Population" ) +
        #adjust the legend
        scale_colour_manual(name = "Landslide Size", values = c("catastrophic" = "darkred", "very_large" = "red", "large" = "orange", "medium" = "yellow", "small" = "green", "unknown"= "darkgrey"))
    }
    
# end of render plot
    })

  #create data set that can be filtered and grouped by user with reactive function
  data1 <- reactive({
    land.na %>% 
      #select variable
      select("fatality_count", "landslide_size", "country_name", "landslide_category", "landslide_trigger", "landslide_setting") %>% 
      #filter by user input
      filter(country_name == input$filter) %>% 
      #group by user input
      group_by( !!sym (input$group)) %>% 
      #give summary statistics
      summarize(mean = mean(fatality_count), max = max(fatality_count), min = min(fatality_count), sd = sd(fatality_count))
  })
  
  #output the data table using renderdatatable 
  output$summary <- renderDataTable(data1())
  
  #create math output for random forest explanation with mathjax package
  output$r.forest.math <- renderUI({
    withMathJax(
      #create equation for classification
      helpText('For classification problems $$ m = {\\sqrt{p}} $$'),
      #create equation for regression
      helpText('For regression problems $$ m = \\frac{p}{3} $$')
    )
  })
  
  #create math output for mlr explanation with mathjax package
  output$mlr.math <- renderUI({
    withMathJax(
      #create equation for higher order terms
      helpText("Example of multiple linear regression with higher order terms: $$ Y_i = β_0 + β_1x_i + β_2x_i^2 + E_i $$"),
      helpText("Example of multiple linear regression ith interaction terms: $$ Y_i = β_0 + β_1x_{1i} + β_2x_{2i} + β_3 x_{1i}x_{2i} + E_i $$")
    )
  })
  
}

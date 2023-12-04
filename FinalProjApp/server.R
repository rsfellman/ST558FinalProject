# Shiny Server for Final Project
# Rachel Fellman



# load necessary packages

library(shiny)
library(tidyverse)

# read in data with a relative path
water<- read_csv("Global_Landslide_Catalog_Export.csv")




# Define server logic 
function(input, output, session) {

  # include landslide image with renderImage function
  output$image <- renderImage({
    list(src = "landslide.png")
  }, deleteFile = FALSE
  )
  
  

    }

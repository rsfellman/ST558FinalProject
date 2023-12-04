# Shiny ui for Final Project
# Rachel Fellman


# Load necessary packages

library(shiny)
library(tidyverse)

# Read in data with a relative path
water<- read_csv("Global_Landslide_Catalog_Export.csv")




# Define UI for application 
fluidPage(

    # Application title
    titlePanel("Landslides"),
    
    #add panels with tabsetPanel function
    tabsetPanel(
      
      #create first tabPanel for about tab
      tabPanel("About",
               
               #set up the layout for the about tab
               sidebarLayout(
                 #create the main panel for all the text
               mainPanel(
                 
               #describe purpose of app
               h3("Purpose"),
               p("This shiny app is being created for the final project for ST 558. It will plot, model, and predict using a dataset of my chosing. It is created using and R studio project connected to github and the Shiny package."),
               
               # describe data used in app
               h3("Data"),
               p("This app uses the Global Landslide Catalog from NASA's Open Data Portal. This calatog was created to help identify landslide events that were trigered by rainfall and contains 11033 obsesrvations with 31 variables. More information on the data used in this app's creation can be found at the following link:"),
               a("https://data.nasa.gov/Earth-Science/Global-Landslide-Catalog-Export/dd9e-wu2v"),
               
               # describe purpose of each tab
               h3("Tabs"),
               p("The data exploration tab will allow the app's user to creat different kind of numerical and graphical summaries. The modeling tab will fit two types of supervisedl learnin models. The modeling tab will have 3 subtabs as well. The first subtab, modeling info, will explain the two chosen models. The second subtab, model fitting, will be where the data is trained and tested on the model. The last subtab under the modeling tab will be the prediction tab. This will give the app user the ability to predict using the models included in the previous tab. The user will be able to select values for the predictors in this tab.")
                      ),
               
               #create the side panel for the image
                 sidebarPanel(
               #include relevant photo for data. Since this data is about landslides I will include a cartoon image of a landslide.
                 imageOutput("image", width = "100px", height = "100px")
                              )
                            )
            ),
    
    #create Data Exploration Tab
    tabPanel("Data Explotation",
             
             #do data exploration
             
             ),
    
    #create Modeling Tab
    tabPanel("Modeling",
             
             #create subtabs with tabsetpanel function
             tabsetPanel(
               
               #create modeling info subtab
               tabPanel("Modeling Info",
                        
                        p("explain models")
                        
                        ),
               
               #create subtab for model fitting
               tabPanel("Model Fitting",
                        
                        #stuff
                        
                        ),
               
               #create subtab for prediction
               tabPanel("Prediction"
                        
                        #predict
                        
                        )
               
             )
             
             
             )
    )
)

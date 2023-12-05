# Shiny ui for Final Project
# Rachel Fellman


# Load necessary packages

library(shiny)
library(tidyverse)
library(maps)
library(DT)
library(rlang)
library(mathjaxr)

# Read in data with a relative path
landslide<- read_csv("Global_Landslide_Catalog_Export.csv")

#drop missing fatality count observations
land.na <- landslide %>% drop_na(fatality_count)

#make country name a factor
land.na$country_name <- as.factor(land.na$country_name)

#filter US data with filter function
land.us<- land.na %>% 
  filter(country_code == "US")



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
               p("This shiny app is being created for the final project for ST 558. It will plot, model, and predict using a dataset of my chosing (I am going to examine landslide data). It is created using and R studio project connected to github and the Shiny package."),
               
               # describe data used in app
               h3("Data"),
               p("This app uses the Global Landslide Catalog from NASA's Open Data Portal. This calatog was created to help identify landslide events that were triggered by rainfall, and it contains 11033 obsesrvations with 31 variables. More information on the data used in this app's creation can be found at the following link:"),
               a("https://data.nasa.gov/Earth-Science/Global-Landslide-Catalog-Export/dd9e-wu2v"),
               
               # describe purpose of each tab
               h3("Tabs"),
               p("The data exploration tab will allow the app's user to creat different kind of numerical and graphical summaries. The modeling tab will fit two types of supervised learning models and will have 3 subtabs as well. The first subtab, modeling info, will explain the two chosen models. The second subtab, model fitting, will be where the data is trained and tested on the model. The last subtab under the modeling tab will be the prediction tab. This will give the app user the ability to predict using the models included in the previous tab. The user will be able to select values for the predictors in this tab.")
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
             #creat sub panels to separate graphs from numeric summaries
             tabsetPanel(
               tabPanel("Graphical Summaries",
             #create layout for data exploration tab
             sidebarLayout(
               
               #create sidebar panel where user can change inputs
               sidebarPanel(
                 
                 selectInput("graphsum",
                             #add label
                             "Graphical Summary Type", 
                             #add choices for the drop down
                             c("World Map" = 1, "U.S. Map" = 2, "Bar Graph" = 3, "Scatter Plot" = 4), 
                             #set initial value to world map
                             selected = "World Map"),
                 
                 #add conditional so the user can change what is viewed in each graphical summary
                 conditionalPanel(
                   #set condition
                   condition = "input.graphsum == 1",
                   radioButtons("color1",
                                 #add label
                                 "Color Points By:",
                               # add choices for buttons
                               c("Landslide Size" = 1 , "Injuries" = 2),
                               #set initial value
                               selected = 1)
                 ),
                 
                 #add conditional panel for when us map is selected
                 conditionalPanel(
                   #set condition
                   condition = "input.graphsum == 2",
                   radioButtons("color2",
                                #add label
                                "Color Points By:",
                                # add choices for buttons
                                c("Fatalities" = 1 , "Landslide Size" = 2),
                                #set initial value
                                selected = 1)
                 ),
                 
                 #add conditional panel for when bar graph is selected
                 conditionalPanel(
                   #set condition
                   condition = "input.graphsum == 3",
                   radioButtons("color3",
                                #add label
                                "Fill Graph By:",
                                #add choices for buttons
                                c("Landslide Setting" = 1, "Landslide Trigger" =2 , "Landlside Category" = 3),
                                selected = 1)
                 ),
                 
                 #add conditional panel for when scatterplot is selected
                 conditionalPanel(
                   #set condition
                   condition = "input.graphsum == 4",
                   #create check box to remove outlier
                   checkboxInput("outlier",
                                 #add label
                                 "Remove fatality outliers?",
                                 #add initial value
                                 value = FALSE),
                   #create check box to color points by landslide size
                   checkboxInput("color4",
                                 #add label
                                 "Color points by landlside size?",
                                 #add initial value
                                 value = FALSE)
                 )
                 
                 #close sidebar panel
                 ),
               
               
               #create Main Panel
               mainPanel(
                 
                 #add and name the plot output
                 plotOutput("sum.plot")
                 
               )
             )
             #close graphical summaries subtab subtab
               ),
             #create subpanel for numeric summaries
             tabPanel("Numeric Summaries",
                      #create layout for numeric summaries subtab
                      sidebarLayout(
                        # create sidebar panel
                        sidebarPanel(
                          #add filtering choices with select input function
                          selectInput("filter", 
                                      #add label
                                      "Select Country to Filter Data",
                                      #create options
                                      choices = levels(land.na$country_name),
                                      #chose initial value
                                      selected = "United States"),
                          
                          # add grouping choices with select input function
                          selectInput("group",
                                      #add label
                                      "Select Grouping Criteria",
                                      #create grouping options
                                      choices = c("landslide_category", "landslide_trigger", "landslide_size", "landslide_setting"),
                                      #chose initial value
                                      selected = "landslide_category")
                        ),
                        
                        #create main panel
                        mainPanel(
                          
                          #add table output
                          dataTableOutput ("summary")
                        )
                      )
                      
                      
                      
                      )
             # close subtabs
             )
             
#close data exploration tab
             
             ),
    
    #create Modeling Tab
    tabPanel("Modeling",
             
             #create subtabs with tabsetpanel function
             tabsetPanel(
               
               #create modeling info subtab
               tabPanel("Modeling Info",
                        
                        #header
                        h3("Multiple Linear Regression"),
                        p("I will start by modeling the fatality count with a multiple linear regression model. While simple linear regression models one response to one predictor, multiple linear regression takes that a step farther. In multiple linear regression we can include more than one predictor variable, higher order terms (like squared terms), interaction terms, or any combination of those."),
                        
                        #use uioutput to create output that will allow for creation of equations in server
                        uiOutput("mlr.math"),
                        
                        #more about multiple linear regression
                        p("When we include more terms in our model it typically improves our prediction ability (to an extent). Multiple linear regression is one example of this. However, when too many terms are included, our models can become overfit to our data. This is one of the downsides of multiple linear regression. In order to prevent overfitting, a variable selection technique such as forward selection can be used. Another downside of multiple linear regression is its complexity. In comparison to simple linear regression, we lose some of the interpretability of the coeficients (β) especially for higher order and interaction terms."),
                        
                        #header
                        h3("Random Forest Models"),
                        #explain random forest model
                        p("I will also model the fatality rate using a random forest model. A random forest model is an extension of the bagged tree classification model. In a simple classification model tree, binary decisions are mapped. Each branch of the tree is called a node and our final classifications can be seen on the leaves. At each node we see a variable split into 2 directions. These splits continue until we get to the leaves.  In a bagged tree model, the bootstrapping method is used and the data is resampled multiple times to create different test sets and then each test set is trained on the models separately. Then our final prediction is the average of the individual model's predictions. The Random Forest model uses the same bagging method and creates multiple trees from the bootstrap samples and averages the results. The difference is that the random forest method does not use all of the predictors, instead it uses a random subset of predictors for each sample and tree fit. More specifically, in a random forest model, each time there is a split, the predictor to split on is chosen from a random sample of m predictors from the full set of p predictors. At each node, a new sample of m predictors is taken."), 
                        
                        #use uioutput to create output that will allow for creation of equations in server
                        uiOutput("r.forest.math"),

  # paragraph about positives and drawbacks of random forest 
  p("In a random forest model, the algorithm cannot choose from the majority of the existing predictors. This is useful in the case where extra strong predictors exist. A regular bagged tree model will likely use said strong predictor for the first split every time, and our resulting trees will look very similar and likely be highly correlated. The average prediction of many correlated tree models does not lead to much of a reduction in the variance. Forcing the model to choose from a subset of predictors, causes the resulting trees to be less correlated and therefore the random forest method leads to a reduction in variance and more reliable prediction. A basic classification tree can be non-robust and small changes in the data can lead to large alterations in the result. Bagging and random forest models aggregate many decision trees to increase the predictive performance of these models. With more models, we get more accurate predictions. Basic classification trees are very susceptible to being overfit to our data since their accuracy improves with each split. The multiple trees created in a random forest model reduces the chances of overfitting our model making them superior when it comes to prediction. However, we do lose the interpretability of a singular classification tree when we use random forest models.")
                        
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
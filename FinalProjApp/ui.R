# Shiny ui for Final Project
# Rachel Fellman
#12/11/23


# Load necessary packages

library(shiny)
library(tidyverse)
library(maps)
library(DT)
library(rlang)
library(mathjaxr)
library(caret)

# Read in data with a relative path
landslide<- read_csv("Global_Landslide_Catalog_Export.csv")

#drop missing fatality count observations and select columns
land.na <- landslide %>% 
  drop_na(fatality_count) %>% 
  select("landslide_category","landslide_trigger","landslide_size","landslide_setting","fatality_count","injury_count","country_name","admin_division_population","longitude", "latitude")


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
  filter(country_name == "United States")

#filter data set so there are no missing values to use for the models
land.nomiss<- land.na[complete.cases(land.na), ]


# Define UI for application 
fluidPage(

    #Add Application title with titlePanel function
    titlePanel("Landslides"),
    
    #add panels with tabsetPanel function
    tabsetPanel(
      
      #create first tabPanel for about tab
      tabPanel("About",
               
               #set up the layout for the about tab
               sidebarLayout(
                 #create the main panel for all the text
               mainPanel(
                 
               #describe purpose of app and format text using html type function
               h3("Purpose"),
               p("This shiny app is being created for the final project for ST 558. It will plot, model, and predict using a dataset of my chosing (I am going to examine landslide data). It is created using an R studio project connected to github and the Shiny package."),
               
               # describe data used in app and format text using html type functions
               h3("Data"),
               p("This app uses the Global Landslide Catalog from NASA's Open Data Portal. This calatog was created to help identify landslide events that were triggered by rainfall, and it contains 11033 obsesrvations with 31 variables. More information on the data used in this app's creation can be found at the following link:"),
               #add link to data using tags$a() function
               tags$a(href = "https://data.nasa.gov/Earth-Science/Global-Landslide-Catalog-Export/dd9e-wu2v", "Landslide Data"),
               
               # describe purpose of each tab and format text using html type fucntions
               h3("Tabs"),
               #describe about tab
               p("The about tab, where we are now, provides a brief overview for the app."),
               #describe the data exploration tab
               p("The data exploration tab will allow the app's user to creat different kind of numerical and graphical summaries. There are two subtabs under data exploration. One for graphical summaries where the user can select between 4 different graph types and one for numerical summaries where the user can fitler and group data to get differnt group means."),
               #describe the modeling tab
               p("The modeling tab will fit two types of supervised learning models and will have 3 subtabs as well. The first subtab, modeling info, will explain the two chosen models. The second subtab, model fitting, will be where the data is trained and tested on the model. The last subtab under the modeling tab will be the prediction tab. This will give the app user the ability to predict using the models included in the previous tab. The user will be able to select values for the predictors in this tab.")
                      ),
               
               #create the side panel for the image using sidebarPanel function
                 sidebarPanel(
               #include relevant photo for data. Since this data is about landslides I will include a cartoon image of a landslide usinf the inageOutput function.
                 imageOutput("image", width = "100px", height = "100px")
                              )
                            )
            ),
    
    #create Data Exploration Tab with tabPanel function
    tabPanel("Data Explotation",
             #create sub panels to separate graphs from numeric summaries
             tabsetPanel(
               #create subtab for graphical summaries using tabPanel function
               tabPanel("Graphical Summaries",
             #create layout for data exploration tab
             sidebarLayout(
               
               #create sidebar panel where user can change inputs
               sidebarPanel(
                 
                 #create input dropdown using selectInput function
                 selectInput("graphsum",
                             #add label
                             "Graphical Summary Type", 
                             #add choices for the drop down
                             c("World Map" = 1, "U.S. Map" = 2, "Bar Graph" = 3, "Scatter Plot" = 4), 
                             #set initial value to world map
                             selected = "World Map"),
                 
                 #add conditional so the user can change what is viewed in each graphical summary using conditionalPanel function
                 conditionalPanel(
                   #set condition
                   condition = "input.graphsum == 1",
                   #add buttons to allow user to change what the points are colored by using the radioButtons function
                   radioButtons("color1",
                                 #add label
                                 "Color Points By:",
                               # add choices for buttons
                               c("Landslide Size" =1 , "Injuries" =2),
                               #set initial value
                               selected = 1)
                 ),
                 
                 #add conditional panel for when us map is selected 
                 conditionalPanel(
                   #set condition
                   condition = "input.graphsum == 2",
                   #add buttons that allows the user to change what the points are colored by using the radioButtons function
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
                   #add buttons that allow user to select what the graph will be filled by using radioButtons function
                   radioButtons("color3",
                                #add label
                                "Fill Graph By:",
                                #add choices for buttons
                                c("landslide_setting", "landslide_trigger" , "landslide_category"),
                                selected = "landslide_setting")
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
             #close graphical summaries subtab
               ),
             #create subpanel for numeric summaries using tabPanel function
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
                          
                          #close sidebar panel
                        ),
                        
                        #create main panel
                        mainPanel(
                          
                          #add header
                          h3("Average Number of Fatalities for Landslide Events"),
                          
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
                        
                        #describe multiple linear regression and format text using html type functions
                        #header
                        h3("Multiple Linear Regression"),
                        #text
                        p("I will start by modeling the fatality count with a multiple linear regression model. While simple linear regression models one response to one predictor, multiple linear regression takes that a step farther. In multiple linear regression we can include more than one predictor variable, higher order terms (like squared terms), interaction terms, or any combination of those."),
                        
                        #use uioutput to create output that will allow for creation of equations in server
                        uiOutput("mlr.math"),
                        
                        #describe more about multiple linear regression
                        p(" There are a few assumptions we make when fitting a multiple linear regression model: there is a linear relationship between response and predictor variables, the predictors are not highly correlated, and the residuals are normally distributed. When we include more terms in our model, it introduces the possibility for correlation between the predictors. However, more varibles also typically improves our prediction ability. Multiple linear regression is one example of this. On the other hand, when too many terms are included, our models can become overfit to our data. This is one of the downsides of multiple linear regression. In order to prevent overfitting, a variable selection technique such as forward selection can be used. Another downside of multiple linear regression is its complexity. In comparison to simple linear regression, we lose some of the interpretability of the coeficients (β) especially for higher order and interaction terms."),
                        
                        #describe random forest models and format data using html type functions
                        #headerpredictors
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
                        
                        #create layout
                        sidebarLayout(
                          #create sidebar panel
                          sidebarPanel(
                            
                            #create slider input that allows user to select the percentage of data used for the training set
                            sliderInput("test",
                                        #add label
                                        "Choose the percentage of data to use for training the model",
                                        #set minimum for slider
                                        min = 0,
                                        #set maximum for slider
                                        max = 100,
                                        #set initial value
                                        value = 80),
                            
                            #line break
                            br(),
                            
                            #create check box input to allow user to chose variables they want in mlr model
                            checkboxGroupInput("mlr.vars",
                                               #add label
                                               "Select the variables you would like to include in the multiple linear regression model",
                                               #add choices (all variables besides fatality_count which is our response)
                                               names(land.nomiss[,-5]),
                                               # have all variables selected to start
                                               selected = names(land.nomiss[,-5])),
                            
                            #line break
                            br(),
                            
                            #create check box input that allows user to chose variables for random forest model
                            checkboxGroupInput("rforest.vars",
                                               #add label
                                               "Select the variables you would like to include in the random forest model",
                                               #add choices (all variable besides fatality_count which is our response)
                                               names(land.nomiss[,-5]),
                                               # have two variables selected to start
                                               selected = c("landslide_category", "landslide_trigger")
                                               ),
                            
                            
                            #create slider input that allows the user to set the tuning parameter
                            sliderInput("mtry",
                                        #add label
                                        "Set tuning parameter, mtry, for the random forest model",
                                        #set minimum
                                        min = 1,
                                        #set maximum
                                        max = 9,
                                        #set initial value
                                        value = 2),
                            
                            #create numeric input for cross validation
                            numericInput("cv",
                                         #add label
                                         "select the number of folds for cross validation on the random forest model",
                                         #set minimum
                                         min = 1,
                                         #set maximum
                                         max = 10,
                                         #set initial value
                                         value = 3),
                            
                            #add break
                            br(),
                            
                            #create action button
                            actionButton("submit", "Fit Models")
                            
                            
                            
                          ),
                          
                          
                          
                          #create main panel
                          mainPanel(
                            #add paragraph style text instructions for user
                            p("To get results for your model, press the 'Fit Models' button. We are modeling the fatality count based on a variety of predictor variables seen on the sidebar."),
                            
                            #create column layout
                            fluidRow(
                              column(
                                
                                #add bigger title
                                h3("Multiple Linear Regression"),
                            #add bold title
                            strong("Fit Statistics"),
                            
                            #add break
                            br(),
                            
                            #create output for results of mlr training 
                            tableOutput ("models.mlr"),
                            
                            #add bold title
                            strong("Comparison Statistics on Test Data"),
                            
                            #create table output for test statistics
                            verbatimTextOutput("mlr.test"),
                            
                            #add bold title
                            strong("MLR Summary"),
                            
                            #table output for mlr summary
                            verbatimTextOutput("mlr.summary"),
                            
                            
                            
                            #set column width
                            width = 6),
                            
                            #set up other column
                            column(
                              
                              #add bigger title
                              h3("Random Forest"),
                            #add bold titles
                            strong("Fit Statistics"),
                            
                            #add break
                            br(),
                            
                            #create output for results of random forest training
                            tableOutput("models.rf"),
                            
                            #add bold title
                            strong("Comparison Statistics on Test Data"),
                            
                            #create table output for test statistics
                            verbatimTextOutput("rf.test"),
                            
                            #add bold titles
                            strong("Plot of Variable Importance for Top 20 Variables"),
                            
                            #create plot for variable importance
                            plotOutput("rf.imp"),
                            
                            #create conditional panel for text
                            conditionalPanel(
                              #add condition
                              condition = "input.submit",
                            
                              #add paragraph style text to explain some oddities of the variable importance plot
                            p("The variables shown in this plot might seem a bit odd at first glance. This is beauce a large number of the predictor variables used are categorical in nature. In this case R has created a different variable for each level of the categorical variables.")
                            ),
                            
                            
                            
                            #set column width
                            width = 6
                            )
                          )
                          )
                        )
                        
                        ),
               
               #create subtab for prediction
               tabPanel("Prediction",
                        
                        #create layout
                        sidebarLayout(
                          #create sidebar
                          sidebarPanel(
                            
                            #add title
                            h4("Select values of the predictor variables"),
                            
                            
                            
                            #use selectInput to allow user to chose value for landslide_category
                            selectInput("category",
                                        #add label
                                        "landslide_category",
                                        #add choices (all levels of the landslide_category factor variable)
                                        choices = levels(land.nomiss$landslide_category),
                                        #select initial value
                                        selected = "landslide"),
                            
                            #use select input to allow user to chose value for landslide_trigger
                            selectInput("trigger",
                                        #add label
                                        "landslide_trigger",
                                        #add choices (all levels of the landslide_trigger factor variable)
                                        choices = levels(land.nomiss$landslide_trigger),
                                        #select initial value
                                        selected = "downpour"),
                            
                            #use selectInput to allow user to chose values for landslide_size
                            selectInput("size",
                                        #add label
                                        "landslide_size",
                                        #add choices (all levels of the landslide_size factor variable)
                                        choices = levels(land.nomiss$landslide_size),
                                        #select initial value
                                        selected = "small"),
                            
                            #use selectInput to allow user to chose values for landslide_setting
                            selectInput("setting",
                                        #add label
                                        "landslide_setting",
                                        #add choices (all levels of the landslide_setting factor variable)
                                        choices = levels(land.nomiss$landslide_setting),
                                        #select initial value
                                        selected = "above_road"),
                            
                            #use numericInput to allow user to chose input for injury_count
                            numericInput("injury",
                                         #add label
                                         "injury_count",
                                         #add min
                                         min = 0, 
                                         #add max (set max equal to the maximum injury_count in our data set to prevent user from extrapolating)
                                         max = max(land.nomiss$injury_count),
                                         #set initial value
                                         value = 10),
                            
                            
                            
                            #use selectInput to allow user to chose input for country_name
                            selectInput("country",
                                        #add label
                                        "country_name",
                                        #add choices (all levels of the country_name factor variable)
                                        choices = levels(land.nomiss$country_name),
                                        #select initial value
                                        selected = "United States"),
                            
                            #use numericInput to allow user to select input for admin_division_population
                            numericInput("pop",
                                         #add label
                                         "admin_division_population",
                                         #add min (set min equal to the minimum admin_division_population to prevent user from extrapolating)
                                         min = min(land.nomiss$admin_division_population),
                                         #add max (set max equal to the maximum admin_division_population to prevent user from extrapolating)
                                         max = max(land.nomiss$admin_division_population),
                                         #set initial value
                                         value = 2000),
                            
                            #use numericInput to allow user to select input for longitude
                            numericInput("longitude",
                                         #add label
                                         "longitude",
                                         #add min
                                         min = -180,
                                         #add max
                                         max = 180,
                                         #set initial value to my home city (Charlotte)
                                         value = -81),
                            
                            #use numericInput to allow user to select input for latitude
                            numericInput("latitude",
                                         #add label
                                         "latitude",
                                         #add min
                                         min = -90,
                                         #add max
                                         max = 90,
                                         #set initial value to my home city (Charlotte)
                                         value = 35),
                            
                            #add break
                            br(),
                            
                            #use actionButton to create button for user to select to predict
                            actionButton("predict",
                                         #add label
                                         "Predict"),
                            
                            
                            
                            ), 
                            
                          
                          
                          #create main panel
                          mainPanel(
                            
                            #add text instructions formated using html type functions
                            h5("To predict the fatality count, select values of the predictor variables then click the predict button."),
                            #add break
                            br(),
                            br(),
                            
                            #add text title
                            h4("Predicted fatality count using multiple linear regression model:"),
                            
                            #use verbatimTextOutput to create output for prediction using mlr model
                            verbatimTextOutput("mlr.predict"),
                            
                            #add text title
                            h4("Predicted fatality count using random forest model:"),
                            
                            #use verbatimTextOutput to create output for prediction using random forest model
                            verbatimTextOutput("rf.predict"),
                            
                            #add break
                            br(),
                            
                            #add text to explain models
                            h5("Note: These models are not the best at predicting fatality count and it is possible to get a negative prediction which makes no sense in this context. Also note that certain combinations of longitude/latitude and country name do not realistically exist.")
                            
                            
                          )
                        )
                        
                        )
               
             )
             
             
             )
    )

)

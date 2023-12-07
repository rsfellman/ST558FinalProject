# Shiny Server for Final Project
# Rachel Fellman



# load necessary packages

library(shiny)
library(tidyverse)
library(maps)
library(DT)
library(rlang)
library(mathjaxr)
library(caret)

# read in data with a relative path
landslide<- read_csv("Global_Landslide_Catalog_Export.csv")

#drop missing fatality count observations and select variables
land.na <- landslide %>% 
  drop_na(fatality_count) %>% 
  select("landslide_category","landslide_trigger","landslide_size","landslide_setting","fatality_count","injury_count","country_name","admin_division_population", "longitude", "latitude")

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

#filter data set so there are no missing values
land.nomiss<- land.na[complete.cases(land.na), ]

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
    
    
    #create graph with condition world map colored by size
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
      
      
      #create graph for the conditions bar graph and  fill depending on user input
    } else if (input$graphsum ==3) {
      #create bar plot fill by landslide setting
     g + geom_bar(data = land.na, aes(x = landslide_size, fill = !!sym(input$color3))) +
        #add title
      labs(title = "Bar Graph of Landslide Size")
      
      
      #create graph for conditions scatterplot and no boxes checked
    } else if (input$graphsum == 4 & input$outlier == FALSE & input$color4 == FALSE) {
      #create scatter plot
      g + geom_jitter(data = land.na, aes(y = fatality_count, x = admin_division_population)) +
      #add labels
      labs(title = "Scatter Plot of Landslide Fatality Count by Population", y = "Fatality Count", x = "Population" )
      
      #create graph for conditions scatterplot and outlier box checked
    } else if (input$graphsum == 4 & input$outlier == TRUE & input$color4 == FALSE) {
      g + geom_jitter(data = land.out, aes(y = fatality_count, x = admin_division_population)) +
        #add labels
        labs(title = "Scatter Plot of Landslide Fatality Count by Population", y = "Fatality Count", x = "Population" )
      
      #create graph for conditions scatter plot and outlier and size box checked
    } else if (input$graphsum == 4 & input$outlier == TRUE & input$color4 == TRUE) {
      g + geom_jitter(data = land.out, aes(y = fatality_count, x = admin_division_population, color = landslide_size)) +
        #add labels
        labs(title = "Scatter Plot of Landslide Fatality Count by Population", y = "Fatality Count", x = "Population" ) +
      #adjust the legend
    scale_colour_manual(name = "Landslide Size", values = c("catastrophic" = "darkred", "very_large" = "red", "large" = "orange", "medium" = "yellow", "small" = "green", "unknown"= "darkgrey"))
      
      #create graph for conditions scatter plot and size box checked
    } else if (input$graphsum == 4 & input$outlier == FALSE & input$color4 == TRUE) {
      g + geom_jitter(data = land.na, aes(y = fatality_count, x = admin_division_population, color = landslide_size)) +
        #add labels
        labs(title = "Scatter Plot of Landslide Fatality Count by Population", y = "Fatality Count", x = "Population" ) +
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
  
  #models
  
  #use reactive function to get user input on split for train/test set.
  train.test <- reactive({
    # create training index
    train.index<- createDataPartition(land.nomiss$fatality_count, p = (input$test/100), list = FALSE)
    #create train and test sets
    landslide.train <- land.nomiss[train.index, ]
    landslide.test <- land.nomiss[-train.index, ]
    #create list output
    list(land.train = landslide.train, land.test = landslide.test)
  })
  

  
  #reevaluate with inputs only when button is pressed using event reactive
  model<- eventReactive(input$submit,{
    #call training set from reactive function
    sets <- train.test()
    
    #filter training set for mlr to include only selected variables
    land.mlrvar <- sets$land.train %>% 
      select("fatality_count",input$mlr.vars)
    
    #filter test set for mlr to include only selected variables
    land.mlrvar.test <- sets$land.test %>% 
      select("fatality_count", input$mlr.vars)
    
    #filter training set for random forest to include only selected variables
    land.rfvar<- sets$land.train %>% 
      select("fatality_count", input$rforest.vars)
    
    #filter test set for random forest to include only selected variables.
    land.rfvar.test <- sets$land.test %>% 
      select("fatality_count", input$rforest.vars)
    
    #put all inputs into a list
    list(mlr = land.mlrvar, rfor = land.rfvar, mtry = input$mtry, cv = input$cv, mlr.test = land.mlrvar.test, rfor.test = land.rfvar.test)
    
  })
  
  #fit models within reactive fucntion
  fitted.models <- reactive({
    #call inputs from eventReactive
    mod<- model()
    
    #fit mlr model
    fit.mlr <- train(fatality_count ~ . , data = mod$mlr,
                     #select method
                     method = "lm",
                     #preprocess data
                     preProcess = c("center","scale"),
                     #do cross validation
                     trControl = trainControl(method = "cv", number = 5))
    
    #fit random forest model
    fit.rf <- train(fatality_count ~ . , data = mod$rfor,
                    #select method
                    method = "rf",
                    #do cross validation
                    trControl = trainControl(method = "cv", number = mod$cv),
                    #add tuning parameter
                    tuneGrid = data.frame(mtry = 1:mod$mtry))
    
    list(fit.mlr = fit.mlr, fit.rf = fit.rf)
  })
  
  #print resutls
  output$models.mlr<-  renderTable({
    data <- fitted.models()
    #print results of mlr fit
    print(data$fit.mlr)
  })
  
  #use renderPrint to create output of fit statistics of mlr model on testing data set.
  output$mlr.test <- renderPrint({
    #call data from reactive function where test and training subsets were formed
    mod <- model()
    #call data from reactive function where the models were fit
    data<- fitted.models()
    
    #use prediction function to fit with the test set data
    pred.mlr <- predict(data$fit.mlr, newdata = mod$mlr.test)
    #use postResample function to get statisctics on test set
    postResample(pred.mlr, obs = mod$mlr.test$fatality_count)
  })
  
  
  output$mlr.summary<- renderPrint({
    data<- fitted.models()
    summary(data$fit.mlr)
  })
  

  
  
  #print results
  output$models.rf <- renderTable({
    data<- fitted.models()
    #print results of random forest model
    print(data$fit.rf)
  })
  
  output$rf.test <- renderPrint({
    #call data from reactive function where test and training subsets were formed
    mod <- model()
    #call data from reactive function where the models were fit
    data<- fitted.models()
    
    #use prediction function to fit with the test set data
    pred.rf <- predict(data$fit.rf, newdata = mod$rfor.test)
    #use post resample function to get statistics on test set
    postResample(pred.rf, obs = mod$rfor.test$fatality_count)
  })
  
  #Use the renderPlot function to create a plot of variable importance for the random forest model.
  output$rf.imp <- renderPlot({
    #get data from previous functions were models were trained
    data<- fitted.models()
    #get variable importance with varimp function
    importance <- varImp(data$fit.rf, scale = FALSE)
    #plot importance for top 20 variables
    plot(importance, top = 20)
  })
  
  
}





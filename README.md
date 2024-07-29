ReadMe
================
Rachel Fellman

# Description
This repository holds a shiny app created in R. The app allows the user to plot, model, and predict data from the Global Landslide Catalog from NASA's Open Data Portal.

## Tabs
The data exploration tab will allow the app's user to create different kind of numerical and graphical summaries. There are two subtabs under data exploration. One for graphical summaries where the user can select between 4 different graph types and one for numerical summaries where the user can filter and group data to get different group means.

The modeling tab will fit two types of supervised learning models and will have 3 subtabs as well. The first subtab, modeling info, will explain the two chosen models. The second subtab, model fitting, will be where the data is trained and tested on the model. The last subtab under the modeling tab will be the prediction tab. This will give the app user the ability to predict using the models included in the previous tab. The user will be able to select values for the predictors in this tab.

# Required Packages

The following is a list of required packages needed to run the app:  
`tidyverse`  
`shiny`  
`maps`  
`DT`  
`rlang`  
`mathjaxr`  
`caret`  
`randomForest`

# Installing Packages

The following is the code needed to install the necessary packages.

``` r
install.packages("tidyverse")
install.packages("shiny")
install.packages("maps")
install.packages("DT")
install.packages("rlang")
install.packages("mathjaxr")
install.packages("caret")
install.packages("randomForest")
```

# Code to Run App

The following code, copied into R, should run the Shiny app.

``` r
shiny::runGitHub("rsfellman/ST558FinalProject", "<rsfellman>", ref = "main", subdir = "FinalProjApp/")
```

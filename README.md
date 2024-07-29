ReadMe
================
Rachel Fellman

# Description

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

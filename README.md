# MyShinyLibrary
An ensemble of shiny tools. More information can be found in ./www of each app.

To run and test the shiny app, e.g., spatMap, in Rstudio simply do:   
require(shiny)  
runGitHub("MyShinyLibrary", "zhangz19", subdir='spatMap', launch.browser=TRUE)   

Or download the folder, setwd to where the folder locates, do:  
library(shiny)  
runApp("spatMap", launch.browser=TRUE)  

Please install necessary R packages, e.g.:   
install.packages('shiny')  
install.packages('rgdal')  
install.packages('rgeos')  
install.packages('leaflet')  
install.packages('arules')  

List of apps:   
library(shiny)   
runGitHub("MyShinyLibrary", "zhangz19", subdir='SimonOC', launch.browser=TRUE)  



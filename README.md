# R-Fallow

The R/Shiny implementation of original FALLOW model developed by ICRAF

## How to run

Run the program from this source code by executing this script in R or RStudio:

    library(shiny)
    runGitHub("rfallow", "degi")

This Shiny app requires R software with a minimum version of 4.3.0. The required library will be automatically installed on R when the script is run for the first time. In case of an error in the library installation, manual installation may be required. The required libraries are as follows:  

* shiny
* bs4Dash
* shinyWidgets
* shinyjs
* shinyjqui
* RColorBrewer
* areaplot
* fresh
* excelR
* thematic
* sf
* stars
* mapview
* leaflet
* leafem
* utility
* dplyr
* reshape
* yaml
* openxlsx2
* markdown 

## Online demo

Online demo can be found on free server here: https://degi.shinyapps.io/rfallow/. Somehow, this server has a limitation of memory, thus running the simulation with big map input is impossible. Running the app from the source code (above) is recommended.

We were planning to get a proper server to host the app. Soon we get a better server, the app address will be shown here. 

**R-Fallow** is the R/Shiny implementation of the original FALLOW model developed by ICRAF

## How to run

Run the program from this GitHub source code using **R** or **RStudio** by executing the script below:

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

The online demo can be found here: https://degi.shinyapps.io/rfallow/. Since this was hosted on a free server with a limitation of memory, running the simulation with big spatial data input will eventually make it crash. So, it is recommended to run the app from the source code.

We were planning to get a proper server to host the app. Soon we get a host server, the app address will be shown here. 

#run rfallow GUI

library(shiny)
d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(d, "/.."))
runApp("rfallow")

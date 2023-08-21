#run RFallow with script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("RFallow_main.R")
source("RFallow_funcs.R")
source("params.R")


# f <- "C:/degi/phil/fallow_davao_tif_310523/fallow8AF_tif.mod"
# d <- "C:/degi/phil/fallow_davao_tif_310523"

f <- "C:/degi/fallow_pagaralam/fallow8AF.mod"
d <- "C:/degi/fallow_pagaralam"



fs <- list.files(d, recursive = TRUE)
filepath_list <- paste(d, fs, sep = "/")

par_df <- loadPCRasterFallowParamDef(f)
params <- loadFallowParams(par_df, filepath_list)
  
output <- runRFallow(params, 2)

# do <- summarize_list(output$out_data$lcarea)
# library(areaplot)
# year <- c(1:nrow(do))
# do[is.na(do)] <- 0
# areaplot(year, do,
#          main = "Stacked area chart",
#          xlab = "Year")
# 
# nlab <- summarize_list(output$out_data$nonlaborcosts)
# p <- summarize_list(output$out_data$profit)
# rev <- summarize_list(output$out_data$revenue)

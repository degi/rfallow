library(shiny)
library(stars)
library(shinyjs)
library(shinydashboardPlus)
library(shinyWidgets)
library(openxlsx2)
library(RColorBrewer)
library(areaplot)
library(mapview)
library(leaflet)
library(leafem)
library(dplyr)
library(reshape)

library(randomcoloR)
library(shinyjqui)

source("params.R")
source("RFallow_main.R")


server <- function(input, output, session) {
  options(shiny.maxRequestSize=300*1024^2)
  data_dir = "data_temp"
  v_inp <- reactiveValues(def_file = NULL, def_df = NULL, params_path = NULL,
                          fallow_params = NULL, initlc_file = NULL, 
                          lc_area_df = NULL)
  
  v_out <- reactiveValues(fallow_output = NULL, table_outputs = NULL)
  
  params <- reactiveValues(initlc_map = NULL,
                           map_data_df = NULL,
                           map_list = NULL,
                           landcover_df = NULL, 
                           landuse_df = NULL,
                           livelihood_df = NULL,
                           bio_lc_df = NULL,
                           bio_ll_df = NULL,
                           eco_lc_df = NULL,
                           eco_ll_df = NULL,
                           soc_ll_df = NULL,
                           demographics_df = demographics_df,
                           agentprop_df = agentprop_df,
                           disaster_df = disaster_df,
                           converter_df = converter_df)

  color_list <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"), brewer.pal(12, "Set3"))
  pallete_def <- list()  
  pallete_map <- list()
  
  ##################################################
  ## GENERAL FUNCTION ##############################
  ##################################################
  
  show_alert_file_error <- function(file_error) {
    sendSweetAlert(
      session = session,
      title = "File error!",
      text =  tags$span("Or it was not a", tags$b(file_error), "file!"),
      type = "error", html = TRUE
    )
  }
  
  checkLoadedTable <- function(df, colTypes, ftype) {
    if(is.null(df) | ncol(df) != length(colTypes)) {
      show_alert_file_error(ftype)
      return(F)
    }
    for (i in 1:length(colTypes)) {
      if(colTypes[i] == "c" & !is.character(df[[i]])) {
        show_alert_file_error(ftype)
        return(F)
      } else if(colTypes[i] == "n" & !is.numeric(df[[i]])) {
        show_alert_file_error(ftype)
        return(F)
      }
    }
    return(T)
  }
  
  show_notif_palette <- function() {
    showNotification("Click 'Refresh map' on Map Display to view the effect of palette color changes", type = "warning")
  }
  
  show_alert_nrow_error <- function(nreq, nload) {
    sendSweetAlert(
      session = session,
      title = "Invalid number of rows!",
      text =  tags$span("The required number of rows is ", tags$b(nreq), 
                        ", and the loaded number of rows is ", tags$b(nload)),
      type = "error", html = TRUE
    )
  }
  
  
  
  # open tab menu
  observeEvent(input$start_button, {
    updateTabItems(session, inputId ="sidemenu", selected = "inp_landcover")
  })

  
  ##################################################
  ## INPUT PARAMETRS  ##############################
  ##################################################
  INITLC_FILE <- "initlc.tif"
  INITLC_MAP_VAR <- "initlc_map"
  
  observeEvent(input$upload_parameter, {
    print(paste("Extracting the files:", input$upload_parameter$name))
    update_progress_input(1, "Extracting the files")
    dpath <- input$upload_parameter$datapath
    file_list <- NULL
    try(file_list <- unzip(dpath, list = TRUE), silent = T)
    if(is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }
    unzip(dpath, exdir = data_dir)
    update_progress_input(20, "Uploading scalar parameters")
    p <- lapply(params_file_df$file, function(f){
      fpath <- paste0(data_dir, "/", f)
      if(!file.exists(fpath)) {
        print(paste("File missing:", f))
        return()
      }
      df <- read.csv(fpath)
      if(is.null(df)) return()
      df[df == ""] <- NA
      df
    })
    #TODO: this could be error
    update_progress_input(40, "Uploading land cover initials")
    names(p) <- params_file_df$var
    lcf <- paste0(data_dir, "/", INITLC_FILE)
    if(file.exists(lcf)) {
      p[[INITLC_MAP_VAR]] <- read_stars(lcf)
    } else {
      print(paste("File missing:", INITLC_FILE))
      return()
    }
    
    update_progress_input(50, "Uploading maps")
    if(!is.null(p$map_data_df)) {
      mf <- p$map_data_df$file
      mf <- mf[!is.na(mf) & mf != ""]
      m <- lapply(mf, function(f){
        fpath <- paste0(data_dir, "/", f)
        if(!file.exists(fpath)) {
          print(paste("File missing:", f))
          return()
        }
        suppressWarnings(read_stars(fpath))
      })
      names(m) <- mf
      p$map_list <- m
    }
    apply_parameter(p)
    enable("run_button")
    unlink(data_dir, recursive = TRUE)
    update_progress_input(100, "Parameters upload completed")
  })
  
  observeEvent(input$import_parameter, {
    print(paste("Extracting the files:", input$import_parameter$name))
    update_progress_input(1, "Extracting the files")
    dpath <- input$import_parameter$datapath
    file_list <- NULL
    try(file_list <- unzip(dpath, list = TRUE), silent = T)
    if(is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }
    v_inp$params_path <- paste0(data_dir, "/", file_list$Name)
    unzip(dpath, exdir = data_dir)
    pcraster_files <- grep(pattern = "\\.mod$", x = v_inp$params_path, ignore.case = T, value = T)
    print(pcraster_files)
    flength <- length(pcraster_files)
    if(flength == 1) {
      v_inp$def_file <- pcraster_files[1]
      import_parameter(v_inp$def_file, v_inp$params_path)
    } else if(flength > 1) {
      showModal(modalDialog(
        title = "Multiple files found",
        radioButtons("def_input_option", "Please select the main PCRaster model file:", 
                     choices = pcraster_files),
        footer = tagList(
          actionButton("def_input_select", "OK")
        )
      ))
    } else {
      sendSweetAlert(
        session = session,
        title = "Parameter definition file not found!",
        text = "Please include the parameter definition or PCRaster (.mod) file on the archived parameter files",
        type = "error"
      )
    }
  })

  observeEvent(input$def_input_select, {
    v_inp$def_file <- input$def_input_option
    import_parameter(v_inp$def_file, v_inp$params_path)
    removeModal()
  })

  update_progress_input <- function(value, desc = NULL) {
    updateProgressBar(session, "progress_input", value, title = desc)
  }
  
  import_parameter <- function(pfile, ppath) {    
    def_df <- loadPCRasterFallowParamDef(pfile)
    update_progress_input(5, "Loading the parameters")
    print("Loading the parameters")
    fallow_params <- loadFallowParams(def_df, ppath, update_progress_input)
    apply_parameter(fallow_params)
    #pallete color definiton
    pal_files <- grep(pattern = "\\.pal$", x = ppath, ignore.case = T, value = T)
    lapply(pal_files, function(x){
      suppressWarnings(pallete_def[[basename(x)]] <<- rgb(read.table(x)/255))
    })
    if(!is.null(pallete_def[["lc.pal"]])) {
      pal_df <- data.frame(lc_id = c(0:(length(pallete_def[["lc.pal"]])-1)), 
                           color = pallete_def[["lc.pal"]])
      pallete_map[["initlc_map"]] <<- pal_df
      pallete_map[["lcmap"]] <<- pal_df
    }
    ##############
    enable("run_button")
    print("run enable")
    unlink(data_dir, recursive = TRUE)
  }
  
  apply_parameter <- function(p) {
    #TODO: BUGS -> map table was not updated if the parameters reloaded
    for(n in names(p)) {
      params[[n]] <- p[[n]]
    }
    map_data_df <<- params$map_data_df
    map_row_ids(map_data_df$id)
    is_update_map_conf(T)
    updateMapListBox()
    view_initlc_map()
  }

  output$loadedParams <- renderUI({
    if(is.null(v_inp$def_file)) return()
    tagList(
      HTML(paste("PCRaster file:<b>", basename(v_inp$def_file), "</b>")),
    )
  })
  
  ############################################################
  ### IMPORT FALLOW PARAMS ###################################
  ############################################################

  import_fallow_params <- function(){
    isolate(p <- v_inp$fallow_params)
    
    #Land cover
    params$initlc_map <- p$initlc_map
    lc_df <- p$lc_df[c("lctype", "lcid")]
    names(lc_df) <- c("landcover", "lc_id")
    df <- import_old_lc_par(lc_df)
    params$landcover_df <- generate_LU_IDS(df[lc_field])
    if(!is.null(pallete_map[["initlc_map"]]))
      import_pallete_lc(pallete_map[["initlc_map"]])
    view_initlc_map()
    
    #Map input
    pfile_df <- p$pcraster_params
    mlist <- list()
    
    lapply(general_map_old_ids, function(x){
      f <- pfile_df[pfile_df$parameter == x, "value"]
      m_id <- paste0(x, "_map")
      mlist[[f]] <<- p[[m_id]]
      map_data_df[map_data_df$id == m_id, "file"] <<- f
    })
    apply(suit_map_df, 1, function(x){
      f <- pfile_df[pfile_df$parameter == x["old_id"], "value"]
      m_id <- x["id"]
      mlist[[f]] <<- p$suitability_maps[[m_id]]
      map_data_df[map_data_df$id == x["new_id"], "file"] <<- f
    })
    apply(dist_map_df, 1, function(x){
      f <- pfile_df[pfile_df$parameter == x["fid"], "value"]
      m_id <- x["map_type"]
      mlist[[f]] <<- p$map_data_a[[m_id]]
      map_data_df[map_data_df$id == x["new_id"], "file"] <<- f
    })
    add_map_list(mlist)
    is_update_map_conf(T)
    
    #Scalar input
    lc_df <- params$landcover_df
    pb1 <- p$parbio1_df
    pb1[c("lctype", "luid", "lutype")] <- NULL
    names(pb1)[names(pb1) == "lcid"] <- "lc_id"
    pb1 <- merge(pb1, lc_df[c("lc_id", "lc_short")], by = "lc_id")
    params$bio_lc_df <- pb1

    ll_df <- params$livelihood_df
    pb2 <- p$parbio2_df
    pb2$ll_id <- pb2$livelihoodid + 1
    pb2[c("livelihoodid", "livelihoodtype")] <- NULL
    pb2 <- merge(pb2, ll_df[c("ll_id", "ll_short")], by = "ll_id")
    params$bio_ll_df <- pb2

    pe1 <- p$pareco1_df
    pe1$ll_id <- pe1$livelihoodid + 1
    pe1[c("livelihoodid", "livelihoodtype")] <- NULL
    pe1 <- merge(pe1, ll_df[c("ll_id", "ll_short")], by = "ll_id")
    params$eco_ll_df <- pe1

    nlc <- p$nonlaborcoststat_df[p$nonlaborcoststat_df$mean > 0,]
    nlc <- nlc[c("lcid", "mean", "cv")]
    names(nlc) <- c("lc_id", "nonlaborcost.mean", "nonlaborcost.cv")
    lc_df_id <- lc_df[c("lc_id", "lc_short")]
    pe2 <- merge(lc_df_id, nlc, by = "lc_id", all.x = T)
    ys <- p$yieldstat_df[p$yieldstat_df$mean > 0,]
    ys <- ys[c("lcid", "mean", "cv")]
    names(ys) <- c("lc_id", "yield.mean", "yield.cv")
    pe2 <- merge(pe2, ys, by = "lc_id", all.x = T)
    pe2[is.na(pe2)] <- 0
    params$eco_lc_df <- pe2

    params$demographics_df$value <- p$demographics_df$value
    params$agentprop_df$value1 <- as.vector(p$agentprop_df[1,2:5])
    params$agentprop_df$value2 <- as.vector(p$agentprop_df[2,2:5])
    params$disaster_df[4, "value"] <- p$disaster_time
    if(!is.null(p$disastersocialimpact_df)) {
      params$disaster_df[1:3, "value"] <- disastersocialimpact_df$value
    }
    if(!is.null(p$unitconverter_df)) {
      params$converter_df$value <- p$unitconverter_df$value
    }
  }

  ###################################
  observeEvent(input$upload_initlc, {
    print("Uploading land cover map file")
    v_inp$initlc_file <- input$upload_initlc$datapath
  })

  ###########################################
  create_xltable <- function(df, colset_df) {
    excelTable(data=df, columns = colset_df,
               allowDeleteColumn = F, allowRenameColumn = F, columnSorting = T,
               allowInsertRow = F, allowDeleteRow = F)
  }
  
  
  ##################################################
  ## LAND COVER ####################################
  ##################################################
  
  ##  INPUT Initial land cover map ###########
  observe({
    if(is.null(v_inp$initlc_file)) return()
    m <- NULL
    try(m <- read_stars(v_inp$initlc_file))
    if(is.null(m)) {
      show_alert_file_error("Map")
      return()
    }
    if(is(m[[1]], "factor")) {
      m <- map_factor_to_numeric(m)
    }
    lc_id <- unique(as.vector(m[[1]]))
    if(length(lc_id) < 200) {
      lc_id <- lc_id[!is.na(lc_id)]
      m_df <- data.frame(lc_id)
      isolate(df <- params$landcover_df)
      if(is.null(df)) {
        df <- data.frame(matrix(ncol = length(lc_field), nrow = 0))
        names(df) <- lc_field
      } 
      df <- merge(df, m_df, by = "lc_id", all = T)
      if(all(is.na(df$color))) {
        df$color <- hcl.colors(nrow(df), 'Spectral')
      }
      params$landcover_df <- df
    }
    params$initlc_map <- m
    view_initlc_map()
    enable("run_button")
  })
  
  ## Display LC map
  view_initlc_map <- function() {
    isolate(df <- params$landcover_df)
    map_color <- NULL
    isolate(m <- params$initlc_map)
    if(is.null(m)) return()
    if(!is.null(df)) {
      lc_id <- unique(as.vector(m[[1]]))
      if(length(lc_id) < 200 & nrow(df) > 0) {
        lc_id <- lc_id[!is.na(lc_id)]
        m_df <- data.frame(lc_id)
        df <- merge(df, m_df, by = "lc_id", all = T)
        if(all(is.na(df$color))) {
          lu$color <- hcl.colors(nrow(df), 'Spectral')
        } else {
          df$color[is.na(df$color) | df$color == ""] <- "#000000"
        }
        map_color <- df[c("lc_id", "color")]
        map_color <- map_color[!is.na(map_color$lc_id),]
        map_color <- map_color[!duplicated(map_color$lc_id),]
      }
    }
    # generate land use map
    m_lu <- NULL
    isolate(lu_df <- params$landuse_df)
    if(!is.null(lu_df)) {
      lclu_df <- df[c("lc_id", "lu_id")]
      lclu_df <- lclu_df[lclu_df$lc_id != "" & !is.na(lclu_df$lc_id) &
                         lclu_df$lu_id != "" & !is.na(lclu_df$lu_id),]
      lclu_df <- unique(lclu_df)
      if(nrow(lclu_df) > 0 & length(unique(lclu_df$lc_id)) == nrow(lclu_df)) {
        m_lu <- reclassify_map(m, lclu_df)
        lu_df$color[is.na(lu_df$color) | lu_df$color == ""] <- "#000000"
      }
    }
    output$inp_initlc_display <- renderUI({
      #check Coordinate Reference System (CRS)
      if(is.na(st_crs(m))) {
        output$mapplot_plain <- renderPlot({
          if(is.null(map_color)) {
            plot(m, key.pos = NULL, main = NULL)
          } else {
            plot(m, col = map_color$color, breaks = c(-1, map_color$lc_id),
                 key.pos = NULL, main = NULL)
          }
        })
        plotOutput("mapplot_plain")
      } else {
        mv <- NULL
        if(is.null(map_color)) {
          suppressMessages(
            mv <- mapview(m, na.color = "#FFFFFF00", layer.name = "Land cover", legend = F)
          )
        } else {
          suppressMessages({
            if(length(map_color$lc_id) == length(unique(map_color$lc_id))) {
              mv_lc <- mapview(m, na.color = "#FFFFFF00", layer.name = "Land cover", legend = F,
                          col.regions = map_color$color, at = c(-1, map_color$lc_id))
              mv <- mv_lc
            }
            if(!is.null(m_lu)) {
              mv_lu <- mapview(m_lu, na.color = "#FFFFFF00", layer.name = "Land use", legend = F,
                               col.regions = lu_df$color, at = c(-1, lu_df$lu_id))
              mv <- mv_lu + mv
            }
          })
        }
        if(!is.null(mv)) output$mapplot <- renderLeaflet(mv@map)
        leafletOutput("mapplot")
      }
    })
    lc_area_df <- as.data.frame(table(m))
    names(lc_area_df) <- c("lc_id", "area") 
    v_inp$lc_area_df <- lc_area_df
  }

  observeEvent(input$refresh_lcmap, view_initlc_map())

  ## LC area bar plot
  output$plot_lc_area <- renderPlot({
    lc_area_df <- v_inp$lc_area_df
    if(is.null(lc_area_df)) return()
    lu_df <- params$landuse_df
    if(is.null(lu_df) ) return()
    lc_df <- params$landcover_df
    if(is.null(lc_df)) return()
    
    lc_df <- merge(lc_df, lc_area_df, by = "lc_id", all.x = T)
    lc_df <- merge(lc_df, lu_df[c("lu_id", "lu_short")], by = "lu_id", all.x = T)
    lc_df <- lc_df[!is.na(lc_df$area),]
    lc_df <- lc_df[!is.na(lc_df$lu_short),]
    if(nrow(lc_df) == 0) return()
    
    exc_ids_df <- lc_area_df[!(lc_area_df$lc_id %in% lc_df$lc_id),]
    sum_ex_f <- format(sum(exc_ids_df$area, na.rm = T), big.mark=",")
    ids_ex_f <- paste(exc_ids_df$lc_id, collapse = ', ')
    lg_label <-  c(growth_stage_list, "None")
    lc_df[is.na(lc_df$growth_stage), "growth_stage"] <- ""

    if(nrow(lc_df[lc_df$growth_stage == "",] > 0))
      lc_df[lc_df$growth_stage == "",]$growth_stage <- "None" 
    bar_df <- as.data.frame(cast(lc_df, growth_stage~lu_short, sum, value = "area"))
    bar_df$growth_stage <- factor(bar_df$growth_stage, levels = lg_label)
    bar_df <- bar_df[order(bar_df$growth_stage),]
    lg_label <- unlist(bar_df$growth_stage)
    bar_df$growth_stage <- NULL
    if(nrow(bar_df) == 0) return()
    coln <- intersect(lu_df$lu_short, colnames(bar_df))
    bar_df <- bar_df[coln]
    sum_sim <- sum(unlist(bar_df), na.rm = T)
    sum_f <- format(sum_sim, big.mark=",")
    output$out_tot_area <- renderUI(tagList(
      tags$b(paste("Total simulated area:", sum_f, "ha")),
      div(paste("Excluded area:", sum_ex_f, "ha")),
      div(paste("Excluded ids:", ids_ex_f))
    ))
    c <- color_list[1:length(lg_label)]
    par(mar = c(8,4,0.5,1), fg="white", col.lab = "white", col.axis	= "white")
    b <- as.matrix(bar_df)
    barplot(b, col = c, border="white", las=3, ylab = "Area (ha)")
    legend("topright", legend = rev(lg_label), fill = rev(c), 
           border = "white", bg = "#00000020", box.lwd = 0, title = "Stages")
  }, bg="transparent")

  ## Create land cover table 
  output$inp_initlc_list <- renderExcel({
    # print(params$landcover_df)
    lc_df <- params$landcover_df[lc_field_display]
    if(!is.null(lc_df)) lc_df <- lc_df[order(lc_df$lc_id),]
    excelTable(data=lc_df, 
               columns = lc_column, tableOverflow = T, tableWidth = "100%",
               allowDeleteColumn = F, allowRenameColumn = F, allowInsertColumn = F,
               minDimensions = c(NA,5), tableHeight = "800", autoIncrement = T,
               csvFileName = "landcover_table", includeHeadersOnDownload = T)

  })

  ## Land cover editing
  observeEvent(input$inp_initlc_list, {
    inp <- input$inp_initlc_list
    if(length(inp$data) == 0) {
      isolate(params$landcover_df <- params$landcover_df[0,])
      return()
    }
    df_input <- excel_to_R(inp)
    names(df_input) <- lc_field_display
    df_input <- transform(df_input, lc_id = as.numeric(lc_id))
    isolate(df <- params$landcover_df)
    params$landcover_df <- generate_LU_IDS(df_input)
    if(!all(df$color %in% df_input$color)) show_notif_palette()
    ids <- df_input[!is.na(df_input$lc_id), "lc_id"]
    if(length(ids) != length(unique(ids)))
      showNotification("There were duplicates on the LU_ID, please revise the input setting", type = "error")
  })
  
  ## load LC setting file ############################
  
  observeEvent(input$load_lc, {
    showModal(modalDialog(
      title = "Load land cover setting file",
      fileInput("upload_lc_setting", NULL, accept = ".csv"),
      footer = NULL, easyClose = T
    ))
  })
  
  observeEvent(input$upload_lc_setting, {
    removeModal()
    fpath <- input$upload_lc_setting$datapath 
    if(is.null(fpath)) return()
    df <- read.csv(fpath)
    # print(df)
    if(!checkLoadedTable(df, c("n", "c", "c", "c", "c", "c"), "Land Cover Setting")) return()
    #update the main LC table
    params$landcover_df <- generate_LU_IDS(df)
    show_notif_palette()
  })
  
  ## Import Land Cover 
  
  observeEvent(input$import_lc, {
    showModal(modalDialog(
      title = "Import land cover setting from previous FALLOW version",
      fileInput("upload_lc_old", NULL, accept = ".par"),
      footer = NULL, easyClose = T
    ))
  })
  
  observeEvent(input$upload_lc_old, {
    removeModal()
    if(is.null(input$upload_lc_old$datapath)) return()
    lc_df <- read.table(input$upload_lc_old$datapath)[-1]
    if(!checkLoadedTable(lc_df, c("c", "n"), "Land Cover")) return()
    names(lc_df) <- c("landcover", "lc_id")
    isolate(current_lc_df <- params$landcover_df)
    df <- import_old_lc_par(lc_df, current_lc_df)
    params$landcover_df <- generate_LU_IDS(df[lc_field])
  })

  ## import LC pallete ############################
  
  observeEvent(input$import_pal, {
    showModal(modalDialog(
      title = "Import map color palette",
      fileInput("upload_lcpall", NULL, accept = ".pal"),
      footer = NULL, easyClose = T
    ))
  })
  
  observeEvent(input$upload_lcpall, {
    removeModal()
    if(is.null(input$upload_lcpall$datapath)) return()
    pal_df <- read.table(input$upload_lcpall$datapath)
    if(!checkLoadedTable(pal_df, c("n", "n", "n"), "Palette")) return()
    cpal <- rgb(pal_df/255)
    cpal_df <- data.frame(color = cpal, lc_id = c(0:(length(cpal)-1)))
    import_pallete_lc(cpal_df)
    show_notif_palette()
  })
  
  import_pallete_lc <- function(cpal_df) {
    isolate(df <- params$landcover_df)
    df$color <- NULL
    df <- merge(df, cpal_df, by = "lc_id", all = T)
    #update the main LC table
    params$landcover_df <- df[lc_field]
  }
  
  ## save LC setting
  
  output$download_lc <- downloadHandler(
    filename = function(){"landcover_setting.csv"}, 
    content = function(fname){
      write.csv(params$landcover_df[lc_field_display], fname, row.names = F)
    }
  )

  ##################################################
  ## LAND USE TABLE ################################
  ##################################################

  ## generate LU and Other correlated tables!
  generate_LU_IDS <- function(lc) { 
    if(nrow(lc) == 0) {
      isolate(params$landuse_df <- params$landuse_df[0,])
      return(lc)
    }

    ## update land use
    lu <- unique(lc[c("landuse", "landcover")])
    lu <- lu[lu$landuse != "" & !is.na(lu$landuse), ]
    if(nrow(lu) == 0){
      isolate(params$landuse_df <- params$landuse_df[0,])
      return(lc)
    }
    short_df <- data.frame(landuse = landuse_list, landuse_short)
    lu <- merge(lu, short_df, by = "landuse", all.x = T)
    lu$lu_group <- lu$landuse 
    lu$landuse <- apply(lu[c("lu_group", "landcover")], 1, combine_lulc)
    lu$lu_short <- apply(lu[c("landuse_short", "landcover")], 1, combine_lulc)
    #set the lu_id
    lu$lu_id <- NA
    m1 <- 0
    m2 <- 0
    luset <- lu[grepl("set", lu$landuse,ignore.case=T),]
    lufor <- lu[grepl("for", lu$landuse,ignore.case=T),]
    if(nrow(luset) > 0) {
      lu[lu$landuse %in% luset$landuse, "lu_id"] <- c(1:nrow(luset))
      m1 <- max(lu$lu_id, na.rm = T)
    }
    if(nrow(lufor) > 0) {
      lu[lu$landuse %in% lufor$landuse, "lu_id"] <- c((m1+1):(m1+nrow(lufor)))
      m2 <- max(lu$lu_id, na.rm = T)
    }
    lu[is.na(lu$lu_id), "lu_id"] <- c((max(m1,m2)+1):nrow(lu))
    lu <- transform(lu, lu_id = as.numeric(lu_id))
    lu <- lu[order(lu$lu_id),]
    #remove unused column
    lu$landcover <- NULL
    lu$landuse_short <- NULL
    ##
    isolate(ludf <- params$landuse_df)
    if(is.null(ludf)) {
      # lu$color <- rainbow(nrow(lu))
      lu$color <- hcl.colors(nrow(lu), 'Spectral')
    } else {
      lu$color <- ludf$color[1:nrow(lu)]
      n <- length(lu$color[is.na(lu$color) | lu$color == ""])
      lu$color[is.na(lu$color) | lu$color == ""] <- randomColor(n)
    }
    ## update lu_id on lc table
    lc$lulc <- apply(lc[c("landuse", "landcover")], 1, combine_lulc)
    lc$lu_id <- NULL
    lc <- merge(lc, lu[c("lu_id", "landuse", "lu_short")], by.x = "lulc", by.y = "landuse", all.x = T)
    lc <- lc[order(lc$lc_id),]
    lc$lc_short <- paste0(lc$lu_short, toupper(substr(lc$growth_stage, 1, 2)))
    # isolate(params$landcover_df <- lc[names(landcover_df)])
    params$landuse_df <- lu
    
    
    ############################################
    ## LIVELIHOOD ##############################
    ############################################
    lu_to_ll <- lu[!lu$lu_group %in% c("Settlement", "Forest"), ]
    ll_df <- data.frame(
      livelihood = c(livelihoodtype_base, lu_to_ll$landuse),
      ll_short = c(livelihoodtype_base, lu_to_ll$lu_short),
      lu_id = c(rep(NA, length(livelihoodtype_base)) ,lu_to_ll$lu_id),
      lu_group = c(livelihoodtype_base_group, lu_to_ll$lu_group))
    ll_df$ll_id = c(1:nrow(ll_df))  
    params$livelihood_df <- ll_df[ll_field]
    
    ## update ll_id on lc table
    lc$ll_id <- NULL
    lc <- merge(lc, ll_df[c("ll_id", "livelihood")], by.x = "lulc", by.y = "livelihood", all.x = T)
    
    ############################################
    ## MAP TABLE ###############################
    ############################################

    ## update list of suitability map input options  
    ll_suit_df <- ll_df[ll_df$lu_group %in% c("Tree-based system", "Agriculture"), ]
    map_suit_df <- ll_suit_df[c("livelihood", "ll_id")]
    names(map_suit_df) <- c("label", "ll_id")
    map_data_df$ll_id <<- as.character(map_data_df$ll_id)
    if(nrow(map_suit_df) > 0) {
      map_suit_df$ll_id <- as.character(map_suit_df$ll_id)
      map_suit_df$group <- rep("suitability", nrow(map_suit_df))
      map_suit_df$id <- paste0("suit_",c(1:nrow(map_suit_df)))
      map_suit_df$file <- NA
      map_data_df$id <<- as.character(map_data_df$id)
      map_data_df <<- map_data_df[map_data_df$group != "suitability",]
      map_data_df <<- rows_append(map_data_df, map_suit_df)
    }

    ## update list of proximity map input options
    ll_dist_df <- ll_df[ll_df$lu_group %in% c("Forest", "Tree-based system", "Agriculture"), ]
    lup <- paste("Processing industry of",  ll_dist_df$livelihood)
    map_dist_df <- data.frame(label = c(map_dist_label_base, lup))
    map_dist_df$group <- rep("proximity", nrow(map_dist_df))
    map_dist_df$id <- paste0("proxi_",c(1:nrow(map_dist_df)))
    map_dist_df$file <- NA
    map_dist_df$ll_id <- c(rep("", length(map_dist_label_base)), ll_dist_df$ll_id)
    map_data_df <<- map_data_df[map_data_df$group != "proximity",]
    map_data_df$id <<- as.character(map_data_df$id)
    map_data_df <<- rows_append(map_data_df, map_dist_df)
    ids <- map_data_df$id
    map_data_df$id <<- factor(ids, levels = ids)
    map_row_ids(map_data_df$id)
    
    ###Biophysic input table here #########
    bio <- lc[c("lc_id", "lc_short")]
    bio_id <- par_scalar_title_df$id[par_scalar_title_df$table == "bio_lc_df"]
    bio_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% bio_id]
    lapply(bio_fl, function(x){
      bio[[x]] <<- 0
    })
    params$bio_lc_df <- bio
    
    eco <- lc[c("lc_id", "lc_short")]
    eco_id <- par_scalar_title_df$id[par_scalar_title_df$table == "eco_lc_df"]
    eco_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% eco_id]
    lapply(eco_fl, function(x){
      eco[[x]] <<- 0
    })
    params$eco_lc_df <- eco

    ##########################################################
    bio_ll <- ll_df[c("ll_id", "ll_short")]
    bio_id <- par_scalar_title_df$id[par_scalar_title_df$table == "bio_ll_df"]
    bio_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% bio_id]
    lapply(bio_fl, function(x){
      bio_ll[[x]] <<- 0
    })
    params$bio_ll_df <- bio_ll
    
    eco_ll <- ll_df[c("ll_id", "ll_short")]
    eco_id <- par_scalar_title_df$id[par_scalar_title_df$table == "eco_ll_df"]
    eco_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% eco_id]
    lapply(eco_fl, function(x){
      eco_ll[[x]] <<- 0
    })
    params$eco_ll_df <- eco_ll
    
    soc_ll <- ll_df[c("ll_id", "ll_short")]
    soc_id <- par_scalar_title_df$id[par_scalar_title_df$table == "soc_ll_df"]
    soc_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% soc_id]
    lapply(soc_fl, function(x){
      soc_ll[[x]] <<- 0
    })
    params$soc_ll_df <- soc_ll
    #########################################################
    return(lc[lc_field])
  }
  
  output$inp_lu <- renderExcel({    
    excelTable(data=params$landuse_df[lu_field_display], 
               columns = lu_column, tableOverflow = T, tableWidth = "100%",
               allowDeleteColumn = F, allowRenameColumn = F, allowInsertRow = F,
               allowDeleteRow = F, tableHeight = "800", rowDrag = F,
               csvFileName = "landuse_table", includeHeadersOnDownload = T)
  })
  
  ## Land use editing
  
  observeEvent(input$inp_lu, {
    df_input <- excel_to_R(input$inp_lu)
    isolate(params$landuse_df$color <- df_input[[2]])
    show_notif_palette()
  })
  
  ## load saved LU pallete file ############################
  
  observeEvent(input$load_saved_lu_palette, {
    showModal(modalDialog(
      title = "Load saved land use color palette",
      fileInput("upload_saved_lu_palette", NULL, accept = ".csv"),
      footer = NULL, easyClose = T
    ))
  })
  
  observeEvent(input$upload_saved_lu_palette, {
    removeModal()
    fpath <- input$upload_saved_lu_palette$datapath 
    if(is.null(fpath)) return()
    isolate(df <- params$landuse_df)
    pal_df <- read.csv(fpath)
    if(!checkLoadedTable(pal_df, c("n", "c"), "saved Land Use Palette")) return()
    df$color <- NULL
    df <- merge(df, pal_df, by = "id", all.x = T)
    #update the main LC table
    params$landuse_df <- df
    show_notif_palette()
  })

  ## import LU palette file ############################
  
  observeEvent(input$import_lu_palette, {
    showModal(modalDialog(
      title = "Import land use color pallete",
      fileInput("upload_lupall", NULL, accept = ".pal"),
      footer = NULL, easyClose = T
    ))
  })
  
  observeEvent(input$upload_lupall, {
    if(is.null(input$upload_lupall$datapath)) return()
    isolate(df <- params$landuse_df)
    pal_df <- read.table(input$upload_lupall$datapath)
    if(ncol(pal_df) != 3 | !all(sapply(pal_df, is.numeric))) {
      removeModal()
      show_alert_file_error("Palette")
      return()
    }
    cpal <- rgb(pal_df/255)
    cpal_df <- data.frame(color = cpal, lu_id = c(0:(length(cpal)-1)))
    df$color <- NULL
    df <- merge(df, cpal_df, by = "lu_id", all.x = T)
    #update the main LC table
    params$landuse_df <- df
    removeModal()
    show_notif_palette()
    
  })
  
  ## save LU palette
  output$download_lu <- downloadHandler(
    filename = function(){"landuse_palette.csv"}, 
    content = function(fname){
      write.csv(params$landuse_df[c("id", "color")], fname, row.names = F)
    }
  )

  output$inp_ll <- renderExcel({
    excelTable(data=params$livelihood_df[ll_field_display], 
               columns = ll_column, tableOverflow = T, tableWidth = "100%",
               allowDeleteColumn = F, allowRenameColumn = F, allowInsertRow = F,
               allowDeleteRow = F, tableHeight = "800", 
               csvFileName = "livelihood_table", includeHeadersOnDownload = T)
  })

  ##################################################
  ## SPATIAL DATA INPUT ############################
  ##################################################
  
  map_row_ids <- reactiveVal(map_data_df$id)
  is_update_map_conf <- reactiveVal(TRUE)
  deleted_map_items <- reactiveVal()
  uploaded_map_config_df <- NULL
  uploaded_map_items <- reactiveVal()
  map_dir <- "map_temp"
  
  ## upload map files
  observeEvent(input$upload_spatial, {
    mlist <- list()
    p <- input$upload_spatial$datapath
    fdups <- c()
    ferrs <- c()
    # isolate(u <- v_inp$uploaded_maps)
    isolate(u <- params$map_list)
    fu <- names(u)
    for(i in 1:length(p)) {
      if(input$upload_spatial$type[i] == "application/x-zip-compressed") {
        file_list <- unzip(p[i], list = TRUE)
        unzip(p[i], exdir = map_dir)
        for(f in file_list$Name) {
          if(f %in% fu) {
            fdups <- append(fdups, f)
          } else {
            m <- NULL
            suppressWarnings(suppressMessages(
              try(m <- read_stars(paste0(map_dir, "/",f)), silent = T)
            ))
            if(is.null(m)) {
              ferrs <- append(ferrs, f)
            } else {
              if(is(m[[1]], "factor")) {
                m <- map_factor_to_numeric(m)
              }
              mlist[[f]] <- m
            }
          }
        }
        unlink(map_dir)
      } else {
        f <- input$upload_spatial$name[i]
        if(f %in% fu) {
          fdups <- append(fdups, f)
        } else {
          m <- NULL
          suppressWarnings(suppressMessages(
            try(m <- read_stars(p[i]), silent = T)
          ))
          if(is.null(m)) {
            ferrs <- append(ferrs, f)
          } else {
            if(is(m[[1]], "factor")) {
              m <- map_factor_to_numeric(m)
            }
            mlist[[f]] <- m
          }
        }
      }
    }
    errm <- ""
    if(length(fdups) > 0) {
      errm <- paste("<p><b>Duplicated files:</b>", paste(fdups, collapse = ", "), "</p>")
    }
    if(length(ferrs) > 0) {
      errm <- paste(errm, "<p><b>Error files:</b>", paste(ferrs, collapse = ", "), "</p>")
    }
    if(errm != "") {
      sendSweetAlert(
        session = session,
        title = "Upload error",
        text =  HTML(errm),
        type = "error", html = TRUE
      )
    }
    add_map_list(mlist)
  })
  
  add_map_list <- function(mlist){
    if(length(mlist) > 0) {
      isolate(u <- params$map_list)
      u <- append(u, mlist)
      fu <- names(u)
      fdif <- setdiff(fu, map_data_df$file)
      uploaded_map_items(fdif)
      params$map_list <- u
    }
  }
  
  observe({
    items <- uploaded_map_items()
    updateOrderInput(session, "map_upload", items = items, item_class = "warning")
  })

  ## Uploaded map box
  output$map_upload_box <- renderUI({
    ids <- get_drop_id(map_row_ids())
    items <- input$map_upload
    tagList(
      orderInput("map_upload", NULL, items = items, width = "100%", connect = c("map_delete", ids),
               placeholder = "The uploaded maps will be listed here...", item_class = "warning"),
      uiOutput("drop_delete")
    )
  })

  ## delete box
  output$drop_delete <- renderUI({
    if(!is.null(deleted_map_items())) deleted_map_items(NULL)
    ids <- get_drop_id(map_row_ids())
    tagList(
      icon("trash-can"),
      orderInput("map_delete", NULL, items = NULL, width = "75%", connect = c("map_upload", ids),
                 placeholder = "Drop here to delete...", class = "delete_box")
    )
  })
  
  ## General map table
  output$general_maps <- renderUI({
    if(is_update_map_conf()) isolate(is_update_map_conf(F))
    tagList(
      h5(map_input_df[map_input_df$box_id == "general","box_desc"]),
      hr(style = "border: 1px solid lightgray"),
      fluidRow(apply(map_data_df[map_data_df$group == "general",], 1, create_map_row)),
      tags$i(map_input_footer)
    )
  })
  
  ## Suitability and proximity map table
  apply(map_input_df[c(2,3),], 1, function(x) {
    output[[paste0(x["box_id"], "_maps")]] <- renderUI({
      ll_df <- params$livelihood_df
      if(is.null(ll_df)) {
        return(paste("Please complete Land Cover parameters to get 
               the input options on", x["box_title"]))
      }
      if(is_update_map_conf()) isolate(is_update_map_conf(F))
      ms_df <- map_data_df[map_data_df$group == x["box_id"],]
      tagList(
        h5(x["box_desc"]),
        hr(style = "border: 1px solid lightgray"),
        isolate(fluidRow(apply(ms_df, 1, create_map_row))),
        tags$i(map_input_footer)
      )
    })
  })
  
  ## generate row of map input
  create_map_row <- function(x){
    co <- c("map_upload", "map_delete",get_drop_id(map_data_df$id))
    f <- x[["file"]]
    drop_id <- get_drop_id(x["id"])
    if(is.na(f)) f <- NULL
    tagList(
      column(6, orderInput(drop_id, x["label"], items = f, 
                           connect = co, 
                           ondblclick = "dbFunction(this)",
                           class = "drop_box", width = "100%",
                           placeholder = "Drag the map here...",
                           item_class = "warning")),
      column(3, plotOutput(get_plot_s_id(x["id"]), width = "100%", height = "80px"), 
             ondblclick = "dbFunction(this)", id = paste0("x", drop_id)),
      column(3, uiOutput(get_sum_id(x["id"]))),
      column(12, hr())
    )}

  ## save the config map  
  lapply(map_input_df$box_id, function(x){
    output[[paste0("download_", x, "_map")]] <- downloadHandler(
      filename = function(){paste0(x,"_map.csv")}, 
      content = function(fname){
        write.csv(map_data_df[map_data_df$group == x, c("label", "id", "file")], 
                  fname, row.names = F, na = "")
      }
    )  
  })

  ## Load general map table
  lapply(map_input_df$box_id, function(x){
    observeEvent(input[[paste0("load_", x, "_map")]], {
      showModal(modalDialog(
        title = paste("Load", x, "map configuration file"),
        fileInput("upload_general_map", NULL, accept = ".csv"),
        footer = NULL, easyClose = T
      ))
    })
    
  })

  observeEvent(input$upload_general_map, {
    removeModal()
    fpath <- input$upload_general_map$datapath 
    if(is.null(fpath)) return()
    load_df <- read.csv(fpath)
    h <- c("id", "file")
    if(!all(h %in% names(load_df))) {
      show_alert_file_error("Map Configuration")
      return()
    }
    load_df <- load_df[load_df$file != "", h]
    names(load_df) <- c("id", "file_new")
    isolate(u <- params$map_list)
    fu <- names(u)
    if(is.null(fu)) {
      errf <- load_df$file_new 
      uploaded_map_config_df <<- NULL
    } else {
      errf <- setdiff(load_df$file_new, fu)
      uploaded_map_config_df <<- load_df[load_df$file_new %in% fu,]
    }
    if(length(errf) > 0) {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_upload",
        title = "Unmatched configuration",
        text =  HTML(paste("<P>The defined files are not exist on the uploaded map list: <b>",
                     paste(errf, collapse = ", "), "</b></p>"),
                     "<i>* please upload the required map file first</i><hr>",
                     "<p>Continue with the remaining file setting?</p>"),
        type = "warning", html = TRUE,
      )
    } else {
      uploadMapConfig()
    }
  })
  
  observeEvent(input$confirm_upload, {
    if(!input$confirm_upload) return()
    uploadMapConfig()
  })
  
  
  ## upload the configuration map file
  uploadMapConfig <- function() {
    if(is.null(uploaded_map_config_df)) return()
    if(nrow(uploaded_map_config_df) == 0) return()
    map_data_df$id <- factor(map_data_df$id, levels = map_data_df$id)
    # join the uploaded config with main table
    df <- merge(map_data_df, uploaded_map_config_df, by = "id", all.x = T)
    # remove the file on the old location
    df$file[df$file %in% df$file_new] <- NA
    df$file_new[is.na(df$file_new)] <- df$file[is.na(df$file_new)]
    df$file <- df$file_new
    df <- df[names(map_data_df)]
    map_data_df <<- df
    ## update the uploaded map box
    isolate(u <- params$map_list)
    fu <- names(u)
    fdif <- setdiff(fu, df$file)
    uploaded_map_items(fdif)
    is_update_map_conf(T)
  } 

  # limit the drop zone to one item only
  validateDropMap <- function(dropped, id_drop_map) {
    id <- gsub("drop_", "", id_drop_map)
    f <- map_data_df[map_data_df$id == id, "file"]
    if(!is.na(f)) dropped <- dropped[dropped != f]
    if(length(dropped) == 0) return()
    # remove the dropped file from other rows
    idm <-map_data_df[!is.na(map_data_df$file) & map_data_df$file == dropped, "id"]
    if(!is.null(idm)) {
      map_data_df[map_data_df$id == idm, "file"] <<- NA
    }
    map_data_df[map_data_df$id == id, "file"] <<- dropped
    # update the UI
    updateMapListBox()
    is_update_map_conf(T)
  }
  
  updateMapListBox <- function() {
    isolate(u <- params$map_list)
    fu <- names(u)
    fdif <- setdiff(fu, map_data_df$file)
    uploaded_map_items(fdif)
  }
  
  get_drop_id <- function(x) {
    return(paste0("drop_", x))
  }
  
  get_plot_s_id <- function(x) {
    return(paste0("plot_s_", x))
  }
  
  get_sum_id <- function(x) {
    return(paste0("hist_s_", x))
  }
  
  observe(
    lapply(get_drop_id(map_row_ids()) , function(x) {
      observeEvent(input[[x]], {
        validateDropMap(input[[x]], x)
      })
    })
  )
  
  observeEvent(input$map_upload, {
    intr <- intersect(map_data_df$file, input$map_upload)
    if(length(intr) > 0) {
      map_data_df[map_data_df$file %in% intr, "file"] <<- NA
    }
    uploaded_map_items(input$map_upload)
  })
  
  observeEvent(input$map_delete, {
    intr <- intersect(map_data_df$file, input$map_delete)
    if(length(intr) > 0) {
      map_data_df[map_data_df$file %in% intr, "file"] <<- NA
    }
    params$map_list[[input$map_delete]] <<- NULL
    deleted_map_items(input$map_delete)
    showNotification(paste(input$map_delete, "deleted"), type = "warning")
  })
  
  observe(lapply(map_row_ids(), function(x) {
    output[[get_plot_s_id(x)]] <- renderPlot({
      if(is.null(params$map_list)) return()
      f <- input[[get_drop_id(x)]]
      if(length(f) == 0) return()
      m <- params$map_list[[f[1]]]
      if(all(is.na(m[[1]]))) return()
      pal <- get_map_color(m)
      suppressWarnings(
        suppressMessages(
          tryCatch({
            plot(m, main = NULL, col = pal, key.pos = NULL, breaks = 'equal')
          }, error=function(cond) {
            plot(m, main = NULL, key.pos = NULL, breaks = 'equal')
          })
          
      ))
    })
  }))
  
  observe(lapply(map_row_ids(), function(x) {
    output[[get_sum_id(x)]] <- renderUI({
      if(is.null(params$map_list)) return()
      f <- input[[get_drop_id(x)]]
      if(length(f) == 0) return()
      m <- as.vector(params$map_list[[f[1]]][[1]])
      if(all(is.na(m))) return()
      suppressWarnings(
        tagList(
          div(paste("Min:", round(min(m, na.rm = T), 2))),
          div(paste("Max:", round(max(m, na.rm = T), 2))),
          div(paste("Avg:", round(mean(m, na.rm = T), 2))),
          div(paste("Med:", round(median(m, na.rm = T),2)))
        )
      )
    })
  }))

  disp_map <- reactiveValues(map = NULL, title = NULL)

  observeEvent(input$double_clicked, {
    dbid <- input$double_clicked
    if(startsWith(dbid, "x")) dbid <- sub('.', '', dbid)
    id <- gsub("drop_", "", dbid)
    f <- input[[dbid]]
    if(is.null(f)) return()
    lab <- map_data_df[map_data_df$id == id, "label"]
    title <- paste0(lab, " [", f, "]")
    disp_map$title <- title
    
    m <- params$map_list[[f]]
    disp_map$map <- generateMapPlot(f, m)
    if(!is.null(input$mapbox)) {
      if(!input$mapbox$visible) updateBox("mapbox", action = "restore")
      if(input$mapbox$collapsed) updateBox("mapbox", action = "toggle")
    }
  })

  generateMapPlot <- function(id, map)  {  
    out_id <- paste0("mapplot_", id)
    pal <- get_map_color(map)
    #check Coordinate Reference System (CRS)
    if(is.na(st_crs(map))) {
      output[[out_id]] <- renderPlot({
        suppressWarnings(
          suppressMessages(plot(map, main = NULL, col = pal, breaks = 'equal')))
      })
      plotOutput(out_id)
    } else {
      suppressMessages({
        mv <- mapview(map, na.color = "#FFFFFF00", layer.name = id, col.regions = pal)
      })
      output[[out_id]] <- renderLeaflet(mv@map)
      leafletOutput(out_id)
    }
  }
  
  output$map_display <- renderUI({
    if(is.null(disp_map$map)) return()
    box(id = "mapbox", title = disp_map$title, disp_map$map, collapsible = T,  width = 12, closable = T)
  })

  # observeEvent(input$mapbox$visible, {
  #   collapsed <- if (input$mapbox$collapsed) "collapsed" else "uncollapsed"
  #   visible <- if (input$mapbox$visible) "visible" else "hidden"
  #   message <- paste("My box is", collapsed, "and", visible)
  #   showNotification(message, type = "warning", duration = 1500)
  # })
  
  ##################################################
  ## SCALAR INPUT ############################
  ##################################################
  get_unit_label <- function(u) {
    if(is.na(u)) return(NULL)
    p("Unit:", u)
  }
  
  apply(par_scalar_title_df, 1, function(x){
    output[[paste0("out_", x["id"])]] <- renderUI({
      b_df <- params[[x["table"]]]
      if(is.null(b_df)) { 
        lclink <- paste0("lc_", x["id"])
        observeEvent(input[[lclink]], {
          updateTabItems(session, inputId ="sidemenu", selected = "inp_landcover")
        })
        tagList(
          HTML(lcsetting_req),
          actionButton(lclink, "Go to Land Cover initialization...")
        )
      } else {
        tagList(
          HTML(x["desc"]),
          get_unit_label(x["unit"]),
          excelOutput(paste0("table_", x["id"]), height = "100%")
        )
      }
    })
  })
  
  
  apply(par_scalar_title_df, 1, function(x){
    output[[paste0("table_", x["id"])]] <- renderExcel({
      b_df <- params[[x["table"]]]
      if(is.null(b_df)) return() 
      fl <- par_scalar_field_df[par_scalar_field_df$id == x["id"],]
      n <- nrow(fl)
      b_column <- data.frame(
        title=c("ID", x["table_id_str_label"], fl$title),
        type=c("numeric", "text", rep("text", n)),
        align = c("right", "left", rep("right", n)),
        readOnly = c(T, T, rep(F, n))
      )
      excelTable(data = b_df[c(x["table_id"], x["table_id_str"], fl$field)], columns = b_column, 
                 allowDeleteColumn = F, allowRenameColumn = F, allowInsertRow = F,
                 allowDeleteRow = F, tableHeight = "800", rowDrag = F,
                 csvFileName = paste0(x["id"], "_table"), includeHeadersOnDownload = T,
                 tableOverflow = T, tableWidth = "100%")
    })
  })
  

  ## scalar editing
  apply(par_scalar_title_df, 1, function(x){
    observeEvent(input[[paste0("table_", x["id"])]], {
      inp <- input[[paste0("table_", x["id"])]]
      df_input <- excel_to_R(inp)
      fl <- par_scalar_field_df[par_scalar_field_df$id == x["id"],]
      names(df_input) <- c("ID", x["table_id_str"], fl$field)
      isolate(b_df <- params[[x["table"]]])
      suppressWarnings(
        b_df[fl$field] <- data.frame(apply(
          df_input[fl$field], 2, function(x) as.numeric(as.character(x))))
      )
      params[[x["table"]]] <- b_df
    })
  })
  
  ## Load scalar data ############################
  apply(par_scalar_title_df, 1, function(x){
    observeEvent(input[[paste0("load_", x["id"])]], {
      showModal(modalDialog(
        title = paste("Upload", x["title"], "table"),
        fileInput(paste0("upload_", x["id"]), NULL, accept = ".csv"),
        footer = NULL, easyClose = T
      ))
    })
  })
  
  apply(par_scalar_title_df, 1, function(x){
    observeEvent(input[[paste0("upload_", x["id"])]], {
      inp <- input[[paste0("upload_", x["id"])]]
      if(is.null(inp$datapath)) return()
      inp_df <- read.csv(inp$datapath)
      fl <- par_scalar_field_df[par_scalar_field_df$id == x["id"],]
      n <- nrow(fl)
      if(!checkLoadedTable(inp_df, c("n", "c", rep("n", n)), x["title"])) {
        removeModal()
        return()
      }
      isolate(b_df <- params[[x["table"]]])
      if(is.null(b_df)) return()
      nreq <- nrow(b_df)
      nload <- nrow(inp_df)
      if(nreq != nload) {
        show_alert_nrow_error(nreq, nload)
        return()
      }
      b_df[fl$field] <- inp_df[c(3:(n+2))]
      params[[x["table"]]] <- b_df
      removeModal()
    })
  })
  
  ## save scalar data
  apply(par_scalar_title_df, 1, function(x){
    output[[paste0("download_", x["id"])]] <- downloadHandler(
      filename = function(){paste0(x["id"], "_table.csv")}, 
      content = function(fname){
        fl <- par_scalar_field_df[par_scalar_field_df$id == x["id"],]
        isolate(b_df <- params[[x["table"]]])
        data_df <- b_df[c(x["table_id"], x["table_id_str"], fl$field)]
        names(data_df) <- c("ID", x["table_id_str_label"], fl$title)
        write.csv(data_df, fname, row.names = F)
      }
    )
  })
  
  ##### OTHER #############
  apply(other_inp_df, 1, function(x){
    output[[paste0("out_", x["id"])]] <- renderUI({
      tagList(
        HTML(x["desc"]),
        excelOutput(paste0("table_", x["id"]), height = "100%")
      )
    })
  })
  
  apply(other_inp_df, 1, function(x){
    output[[paste0("table_", x["id"])]] <- renderExcel({
      b_df <- params[[x["table"]]]
      c_df <- other_inp_col_list[[x["id"]]]
      n <- nrow(c_df)
      b_column <- data.frame(
        title= c_df$label,
        type= rep("text", n),
        align = c("left", "left", rep("right", n-2)),
        readOnly = c(T, T, rep(F, n-2))
      )
      excelTable(data = b_df[c_df$col], columns = b_column, 
                 allowDeleteColumn = F, allowRenameColumn = F, allowInsertRow = F,
                 allowDeleteRow = F, #tableHeight = "200px", 
                 rowDrag = F,  #autoFill=F, autoWidth=FALSE,
                 csvFileName = paste0(x["id"], "_table"), includeHeadersOnDownload = T,
                 tableOverflow = T, tableWidth = "100%")
    })
  })
  
  ## OTHER editing
  apply(other_inp_df, 1, function(x){
    observeEvent(input[[paste0("table_", x["id"])]], {
      inp <- input[[paste0("table_", x["id"])]]
      df_input <- excel_to_R(inp)
      c_df <- other_inp_col_list[[x["id"]]]
      names(df_input) <- c_df$col
      cval <- c_df$col[! c_df$col %in% c("label", "unit")]
      isolate(b_df <- params[[x["table"]]])
      suppressWarnings(
        b_df[cval] <- data.frame(apply(
          df_input[cval], 2, function(x) as.numeric(as.character(x))))
      )
      params[[x["table"]]] <- b_df
    })
  })
  
  ## Load scalar data ############################
  apply(other_inp_df, 1, function(x){
    observeEvent(input[[paste0("load_", x["id"])]], {
      showModal(modalDialog(
        title = paste("Upload", x["title"], "table"),
        fileInput(paste0("upload_", x["id"]), NULL, accept = ".csv"),
        footer = NULL, easyClose = T
      ))
    })
  })
  
  apply(other_inp_df, 1, function(x){
    observeEvent(input[[paste0("upload_", x["id"])]], {
      inp <- input[[paste0("upload_", x["id"])]]
      if(is.null(inp$datapath)) return()
      inp_df <- read.csv(inp$datapath)
      c_df <- other_inp_col_list[[x["id"]]]
      n <- nrow(c_df)
      if(!checkLoadedTable(inp_df, c("c", "c", rep("n", n-2)), x["title"])) {
        removeModal()
        return()
      }
      isolate(b_df <- params[[x["table"]]])
      if(is.null(b_df)) return()
      nreq <- nrow(b_df)
      nload <- nrow(inp_df)
      if(nreq != nload) {
        show_alert_nrow_error(nreq, nload)
        return()
      }
      cval <- c_df$col[! c_df$col %in% c("label", "unit")]
      b_df[cval] <- inp_df[c(3:n)]
      params[[x["table"]]] <- b_df
      removeModal()
    })
  })
  
  ## save scalar data
  apply(other_inp_df, 1, function(x){
    output[[paste0("download_", x["id"])]] <- downloadHandler(
      filename = function(){paste0(x["id"], "_table.csv")}, 
      content = function(fname){
        isolate(b_df <- params[[x["table"]]])
        c_df <- other_inp_col_list[[x["id"]]]
        data_df <- b_df[c_df$col]
        names(data_df) <- c_df$label
        write.csv(data_df, fname, row.names = F)
      }
    )
  })
  
  ###################################################################
  ### Parameters checklist ##########################################
  ###################################################################
  
  get_zero_columns <- function(df) {
    if(is.null(df)) return(NULL)
    if(length(unlist(df)) == 0) return(NULL)
    cols <- which(colSums(df != 0) == 0)
    if(length(cols) == 0) return("-")
    colnames(df[cols])
  }
  
  get_zero_vars <- function(df, f = "field", v = "value", suffix = "") {
    if(is.null(df)) return(NULL)
    vars <- unlist(df[f][df[v] == 0])
    if(length(vars) == 0) return(NULL)
    return(paste0(vars, suffix))
  }
  
  get_empty_map <- function(df) {
    if(is.null(df)) return(NULL)
    df[is.na(df)] <- ""
    df <- df[df$file == "", ]
    if(nrow(df) == 0) return(NULL)
    vars <- paste0(df$group, "-", df$label)
    return(vars)
  }
  
  output$out_summary <- renderUI({
    p <- get_parameter()
    plc <- c("landcover_df", "landuse_df", "livelihood_df")
    ps <- c("bio_lc_df", "bio_ll_df", "eco_lc_df", "eco_ll_df", "soc_ll_df")
    
    tagList(
      fluidRow(column(3, HTML("")), 
               column(6, tags$b("Number of IDs"))),
      apply(params_file_df[params_file_df$var %in% plc,], 1, function(d){
        fluidRow(
          column(3, tags$b(d["label"])),
          column(6, HTML(nrow(p[[d["var"]]])))
        )
      }),
      hr(),
      fluidRow(column(3, tags$b("Spatial data")), 
               column(6, HTML("<b>Empty maps: </b>", 
                              paste(get_empty_map(p[["map_data_df"]]), collapse = ", ")
                              ))),
      hr(),
      fluidRow(column(3, HTML("")), 
               column(6, tags$b("Parameters with zero value"))),
      apply(params_file_df[params_file_df$var %in% ps,], 1, function(d){
        fluidRow(
          column(3, tags$b(d["label"])),
          column(6, HTML(paste(get_zero_columns(p[[d["var"]]]), collapse = ", ")))
        )
      }),
      fluidRow(column(3, tags$b("Others")), 
               column(6, HTML(
        paste(c(get_zero_vars(p[["demographics_df"]]), 
                get_zero_vars(p[["disaster_df"]]),
                get_zero_vars(p[["converter_df"]]),
                get_zero_vars(p[["agentprop_df"]], v = "value1", suffix = "_v1"),
                get_zero_vars(p[["agentprop_df"]], v = "value2", suffix = "_v2")), 
              collapse = ", ")
      ))),
      hr()
    )
  })
  
  ###################################################################
  ### Save all the parameters #######################################
  ###################################################################
  
  output$download_params <- downloadHandler(
    filename = function() {
      paste("rfallow_params.zip")
    },
    content = function(fname) {
      p <- get_parameter()
      setwd(tempdir())
      apply(params_file_df, 1, function(d){
        df <- p[[d["var"]]]
        write.csv(df, d["file"], row.names = F, na = "")
      })
      write_stars(p$initlc_map, "initlc.tif")
      lapply(p$map_list, function(m){
        suppressWarnings(write_stars(m, names(m)))
      })
      fs <- c(params_file_df$file, "initlc.tif", names(p$map_list))
      zip::zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  get_parameter <- function() {
    run_par <- reactiveValuesToList(params)
    ## set map input ##
    run_par$map_data_df <- map_data_df
    f <- map_data_df$file
    f <- f[!is.na(f) & f != ""]
    run_par$map_list <- run_par$map_list[f]
    ## validation ##
    blc <-  run_par[["bio_lc_df"]]
    blc$lctimebound.max[is.na(blc$lctimebound.max)] <- .Machine$integer.max
    run_par[["bio_lc_df"]] <- blc
    return(run_par)
  }
  
  ### RUN SIMULATION #################################################
  observeEvent(input$run_button, {
    #TODO: check the params before running
    isRunSimulation(T)
    disable("run_button")
    disable("simtime")
  })
  
  isRunSimulation <- reactiveVal(FALSE)

  observe({
    if(isRunSimulation()) {
      print(paste("Run:", input$simtime, "iteration"))
      v_out$fallow_output <- runRFallow(get_parameter(),
                                        input$simtime,
                                        update_progress_iteration,
                                        update_progress_detail)
      isRunSimulation(F)
      sendSweetAlert(
        session = session,
        title = "Simulation finished!",
        text =  div(p("Number of iteration: ", input$simtime, "simulation years"),
                    p("Elapsed time: ", format(v_out$fallow_output$time_elapsed))),
        type = "success", html = TRUE
      )
      
      enable("run_button")
      enable("simtime")
    }
  })
  
  observeEvent(input$simtime, {
    updateProgressBar(session, "progress_iteration", 0, input$simtime)
  })
  
  update_progress_iteration <- function(value, desc = NULL) {
    updateProgressBar(session, "progress_iteration", value, 
                      total = input$simtime, title = desc)
  }
  
  update_progress_detail <- function(value, desc = NULL) {
    updateProgressBar(session, "progress_detail", value, title = desc)
  }

  ################################################
  ## OUTPUT SELECTOR #############################
  ################################################
  out_lc_label <- "Land cover area" 
  
  output$output_selector <- renderUI({ 

      div(class = "greenbox",
        fluidRow(
          column(4, 
            pickerInput(
            inputId = "output_select", label = "Output", 
            choices = list(out_lc_label,
                           Total = sort(out_df$label[out_df$table == "out_val_df"]),
                           Livelihood = sort(out_df$label[out_df$table != "out_val_df"]),
                           Maps = sort(out_map_df$label) ))),
          column(2, pickerInput(inputId = "output_width", label = "Width", 
                                choices = list("50%", "100%"))
          ),
          column(2, style="margin-top: 25px;", 
                 actionButton("add_output_button", "Display", icon = icon("laptop-medical"))),
          column(4, div(style = "text-align:center;  background:#A9D08F;
                        padding:10px; border-radius:8px; width:140px; margin:auto;", 
                        tags$b("Save output data"),
                        downloadButton("download_output_table", "Download")))
        )
      )
  })
  
  output$download_output_table <- downloadHandler(
    filename = function() {
      paste("rfallow_output.zip")
    },
    content = function(fname) {
      setwd(tempdir())
      d_out <- v_out$fallow_output
      write.csv(d_out$out_val_df, "out_val.csv", row.names = F, na = "")
      write.csv(d_out$out_lc_df, "out_lc.csv", row.names = F, na = "")
      write.csv(d_out$out_ll_df, "out_ll.csv", row.names = F, na = "")
      files = c("out_lc.csv", "out_ll.csv", "out_val.csv")
      zip::zip(zipfile = fname, files = files)
    },
    contentType = "application/zip"
  )
  
  ######## output box dynamic ################
  
  out_box_list <- reactiveVal(NULL)
  
  observeEvent(input$add_output_button, {
    id <- paste0("out_", as.integer(runif(1) * 10^6))
    ids <- out_box_list()
    if(is.null(ids)) {
      ids <- c(id)
    } else {
      ids <- c(ids, id)
    }
    ui_out <- NULL
    if(input$output_select %in% c(out_df$label, out_lc_label)) {
      odf <- out_df[out_df$label == input$output_select,]
      if(nrow(odf) == 0) odf <- list(unit = "ha", desc = "Total land cover area")
      ui_out <- generate_output_table_box(id, input$output_select, 
                                          input$output_width, odf$unit, odf$desc)
    } else if(input$output_select %in% out_map_df$label) {
      ui_out <- generate_output_map_box(id, input$output_select, input$output_width)
    }
    insertUI(selector = "#add_output", where = "afterEnd", ui = ui_out)
    out_box_list(ids)
  })
  
  observe({
    ids <- out_box_list()
    if(is.null(ids)) return()
    lapply(ids, function(id){
      observeEvent(input[[id]]$visible, {
        if(!input[[id]]$visible) {
          removeUI(selector = paste0("div:has(> #", id,")"))
          ids <- ids[ids != id]
          out_box_list(ids)
        }
      })
    })
  })
  
  #################################################################
  ########### Output table box ####################################
  #################################################################
  msg_nodata <- function(title) paste("No data on", title)
  
  generate_output_table_box <- function(id, title, width, unit = "", desc = "") {
    table_id <- paste0("table_out", id)
    #prepare the table
    d_out <- v_out$fallow_output
    if(is.null(d_out)) {
      showNotification(msg_nodata(title), type = "error")
      return()
    }
    iteration_label <- NULL
    field_id_label <- NULL
    field_id <- NULL
    is_single_value <- F
    if(title == out_lc_label) {
      show(paste0("plotdetailgroup", id))
      d_lc <- d_out[["out_lc_df"]]
      if(nrow(d_lc) == 0) {
        showNotification(msg_nodata(title), type = "error")
        return()
      }
      iteration_label <- as.character(unique(d_lc$iteration))
      field_id <- "lc_short"
      field_id_label <- "Land cover"
      idvar <- c("lc_id", "lu_id", "lc_short", "lu_group", "lu_short")
      df <- reshape(d_lc, timevar = "iteration", idvar = idvar, direction = "wide")
      names(df) <- c(idvar, iteration_label)
      
    } else if (title %in% out_df$label[out_df$table == "out_val_df"]) {
      is_single_value <- T
      d_val <- d_out[["out_val_df"]]
      var <- out_df$var[out_df$label == title]
      iteration_label <- var
      field_id <- "iteration"
      field_id_label <- "Iteration"
      df <- d_val[c("iteration", var)]
    } else {
      disable(paste0("plotdetailgroup", id))
      d_ll <- d_out[["out_ll_df"]]
      if(nrow(d_ll) == 0) {
        showNotification(msg_nodata(title), type = "error")
        return()
      }
      iteration_label <- as.character(unique(d_ll$iteration))
      field_id <- "ll_short"
      field_id_label <- "Livelihood"
      var <- out_df$var[out_df$label == title]
      if(!var %in% colnames(d_ll)) {
        showNotification(msg_nodata(title), type = "error")
        return()
      }
      idvar <- c("ll_id", "ll_short", "lu_group")
      df <- d_ll[c(idvar, "iteration", var)]
      df <- reshape(df, timevar = "iteration", idvar = idvar, direction = "wide")
      names(df) <- c(idvar, iteration_label)
    }
    df[is.na(df)] <- 0 #replace NA with zero
    df <- df[rowSums(df[iteration_label] != 0, na.rm = T) > 0,] #remove zero row
    if(nrow(df) == 0) {
      showNotification(msg_nodata(title), type = "error")
      return()
    }
    
    output[[table_id]] <- renderExcel({
      d <- data_view()
      n <- ncol(d)
      out_column <- data.frame(
        title= c(field_id_label, iteration_label),
        type= c("text", rep("numeric", n-1)),
        align = c("left", rep("right", n-1))
      )
      excelTable(data = d, columns = out_column, editable = F,
                 allowDeleteColumn = F, allowRenameColumn = F, allowInsertRow = F,
                 allowDeleteRow = F, tableHeight = "800", rowDrag = F,
                 csvFileName = paste0(title, "_table"), includeHeadersOnDownload = T,
                 tableOverflow = T, tableWidth = "100%")
    })
    
    data_view <- reactiveVal(df)
    filter_id <- paste0("plotfilter", id)
    
    observeEvent(input[[paste0("plotgroup", id)]], {
      if(is_single_value) return()
      updatePrettyCheckbox(session, paste0("plotdetailgroup", id), value = F)
      d <- df[c(field_id, iteration_label)]
      if(input[[paste0("plotgroup", id)]]) {
        d <- df[c("lu_group", iteration_label)]
        d <- aggregate(d[iteration_label], by = list(d$lu_group), FUN = sum, na.rm = T)
        colnames(d)[1] <- "lu_group"
      }
      updatePickerInput(session, inputId = filter_id, choices = d[[1]])
      data_view(d)
    })
    
    observeEvent(input[[paste0("plotdetailgroup", id)]], {
      if(is_single_value) return()
      if(title != out_lc_label) return()
      updatePrettyCheckbox(session, paste0("plotgroup", id), value = F)
      d <- df[c(field_id, iteration_label)]
      if(input[[paste0("plotdetailgroup", id)]]) {
        d <- df[c("lu_short", iteration_label)]
        d <- aggregate(d[iteration_label], by = list(d$lu_short), FUN = sum, na.rm = T)
        colnames(d)[1] <- "lu_short"
        c_df <- merge(d, params$landuse_df[c("lu_short", "lu_id")], 
                      by = "lu_short", all.x = T)
        c_df <- c_df[order(c_df$lu_id),]
        c_df$lu_id <- NULL
        d <- c_df
      }
      updatePickerInput(session, inputId = filter_id, choices = d[[1]])
      data_view(d)
    })

    observeEvent(input[[filter_id]], {
      isolate(g <- input[[paste0("plotgroup", id)]])
      isolate(dg <- input[[paste0("plotdetailgroup", id)]])
      if(g) {
        d <- df[c("lu_group", iteration_label)]
        d <- aggregate(d[iteration_label], by = list(d$lu_group), FUN = sum, na.rm = T)
        colnames(d)[1] <- "lu_group"
      } else if(dg) {
        d <- df[c("lu_short", iteration_label)]
        d <- aggregate(d[iteration_label], by = list(d$lu_short), FUN = sum, na.rm = T)
        colnames(d)[1] <- "lu_short"
      } else {
        d <- df[c(field_id, iteration_label)]
      }
      if(!is.null(input[[filter_id]])) {
        d <- d[d[[1]] %in% input[[filter_id]],]
      }
      data_view(d)
    })
    
    output[[paste0("plot_out", id)]] <- renderPlot({
      d <- data_view()
      if(is.null(d)) return()
      if(nrow(d) == 0) return()
      if(is_single_value) {
        d_plot <- d[iteration_label]
        color <- color_list[1]
      } else {
        dff <- d
        d_plot <- t(dff[iteration_label])
        colnames(d_plot) <- unlist(dff[1])
        color <- color_list[1:nrow(dff)]
        
        idfield <- colnames(d[1])
        if(idfield == "lc_short") {
          c_df <- merge(d[1], params$landcover_df[c("lc_short", "color")], 
                        by = "lc_short", all.x = T, sort = F)
          color <- c_df$color
        } else if(idfield == "lu_short") {
          c_df <- merge(d[1], params$landuse_df[c("lu_short", "color")], 
                        by = "lu_short", all.x = T, sort = F)
          color <- c_df$color
        }
      }
      ex <- 10^(round(log(max(d_plot), 10))-1)
      ex_f <- ""
      if(ex > 100) {
        d_plot <- d_plot/ex
        ex_f <- paste0("[", format(ex, big.mark=",", scientific = F), "]")
      }
      par(mar=c(4, 4, 1, 1))
      ptype <- input[[paste0("plottype_out", id)]]
      if(ptype == "Stacked area" & !is_single_value) {
        areaplot(d_plot, xlab = "Years", ylab = paste(unit, ex_f), col = color, legend = T,
                 args.legend=list(x="topleft", cex=0.8, bty = "o", ncol = 2,
                                  border = "white", bg = "#FFFFFF60", 
                                  box.lwd = 0, inset=.05))
      } else {
        matplot(d_plot, type = "b", pch=16, lty = 1,  col = color,
                xlab = "Years", ylab = paste("Unit", ex_f))
        if(!is_single_value)
          legend("topleft", legend = unlist(d[1]), fill= color, ncol = 2, 
                 cex=0.8, border = "white", bg = "#FFFFFF60", box.lwd = 0,
                 inset=.05)
      }
    }) 
    
    output[[paste0("download_out", id)]] <- downloadHandler(
      
      filename = function() {
        paste(paste0("data-", title, "-"), Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        d <- data_view()
        write.csv(d, file, na = "", row.names = F)
      }
    )
    
    box(id = id, title = title, closable = T, collapsible = T,
        width = ifelse(width == "100%", 12, 6),
        dropdownMenu = boxDropdown(icon = icon("download"),
           boxDropdownItem(
             downloadLink(paste0("download_out", id), "Download data (.csv)"))
        ), 
        sidebar = boxSidebar(
          id = paste0("plotsidebar", id),
          background = COLOR_DARK,
          prettyRadioButtons(
            inputId = paste0("plottype_out", id), label = "Plot type", 
            choices = c("Stacked area", "Line"), selected = "Stacked area",
            icon = icon("check"), inline = TRUE, status = "warning", animation = "jelly"
          ),
          tags$b("Group by"),
          prettyCheckbox(
            inputId = paste0("plotgroup", id), label = "Land use type", 
            icon = icon("check"), status = "warning", animation = "jelly"
          ),
          prettyCheckbox(
            inputId = paste0("plotdetailgroup", id), label = "Detailed land use", 
            icon = icon("check"), status = "warning", animation = "jelly"
          ),
          pickerInput(
            inputId = paste0("plotfilter", id),
            label = "Filter", 
            choices = NULL,
            multiple = TRUE
          )
        ),
        tagList(
          HTML(desc),
          tabsetPanel(
            tabPanel("Plot", icon = icon("chart-simple"), 
                     plotOutput(paste0("plot_out",id))),
            tabPanel("Table", icon = icon("table-list"), 
                     p("Unit:", unit), excelOutput(table_id, height = "100%"))
          )
        )
    )
  }

  ########### Output Map Box #######################
  
  generate_output_map_box <- function(id, title, width) {
    m_id <- out_map_df$id[out_map_df$label == title]
    d_out <- v_out$fallow_output
    if(is.null(d_out)) return()
    m_list <- d_out[["out_map"]]
    m <- m_list[[m_id]] 

    output[[paste0("map_out",id)]] <- renderPlot({
      t <- input[[paste0("slidertime", id)]]
      t <- min(t, length(m))
      map_color <- NULL
      if(title == "Land cover") {
        map_color <- params$landcover_df[c("lc_id", "color")]
        map_color$id <- map_color$lc_id
        map_color <- map_color[order(map_color$id), ]
      } else if(title == "Land use") {
        map_color <- params$landuse_df[c("lu_id", "color")]
        map_color$id <- map_color$lu_id
        map_color <- map_color[order(map_color$id), ]
      }
      if(!is.null(map_color)) {
        map_color <- map_color[map_color$color != "",]
        map_color <- map_color[!is.na(map_color$color),]
        if(nrow(map_color) == 0) map_color <- NULL
      }
      if(is.null(map_color)) {
        pal <- get_map_color(m[t])
        suppressWarnings(suppressMessages(
          plot(m[t], main = NULL, col = pal, breaks = 'equal')))
      } else { 
        plot(m[t], col = map_color$color, breaks = c(-1, map_color$id), main = NULL)
      }
    }) 
    
    output[[paste0("download_tif_out", id)]] <- downloadHandler(
      filename = function() {
        paste0("map_out_", title,".zip")
      },
      content = function(fname) {
        setwd(tempdir())
        files <- c()
        for(i in 1:length(m)){
          f <- paste0(title, "-", i, ".tif")
          files <- c(files, f)
          suppressWarnings(write_stars(m[i], f))
        }
        zip::zip(zipfile=fname, files=files)
      },
      contentType = "application/zip"
    )
    
    box(id = id, title = title, closable = T, collapsible = T,
        width = ifelse(width == "100%", 12, 6),
        dropdownMenu = 
          boxDropdown(
            icon = icon("download"),
            boxDropdownItem(downloadLink(paste0("download_tif_out", id),
                                         "Download the maps"))
        ),
        plotOutput(paste0("map_out",id)),
        sliderInput(paste0("slidertime",id), "Simulation year", 
                    min = 1, max = input$simtime, 
                    value = input$simtime, step = 1)
    )
  }

}


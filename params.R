## RFallow Parameters ##
source("global.R")

loadPCRasterFallowParamDef <- function(pcraster_file) {
  con <- file(pcraster_file)
  return(loadPCRasterFallowParamDefCon(con))
}

loadPCRasterFallowParamDefCon <- function(con) {
  par_df <- data.frame(parameter=character(), value=character())
  lines <- readLines(con, warn=FALSE)
  for (line in lines){
    l <- gsub(";","",trimws(line))
    if (startsWith(l, "areamap")) break;
    if (startsWith(l, "#") || !grepl("=", l, fixed = TRUE)) next;  
    
    lvar <- strsplit(l,split='=')
    par = lvar[[1]][1]
    
    #remove "indexscalar()" characters
    val = gsub("\\)", "", gsub("\\(", "", gsub("indexscalar", "", lvar[[1]][2])))
    
    #store the parameters into data frame
    par_df[nrow(par_df)+1,] <- c(par, val)  
  }
  close(con)
  return(par_df)
}

loadFallowParams <- function(par_df, filepath_list, progress = NULL) {
  params <- list()
  errors <- c()
  
  params[["pcraster_params"]] <- par_df
  
  getInputFile <- function(parameter) {
    fname <- par_df[par_df$parameter == parameter,][[2]]
    getFilePath(fname)
  }
  
  getFilePath <-  function(fname) {
    fpath <- grep(pattern = paste0("/",fname,"$"), x = filepath_list, ignore.case = T, value = T)
    if(length(fpath) == 0) return(NULL)
    return(fpath)
  } 
  
  getInputMap <- function(parameter) {
    input_file <- getInputFile(parameter)
    if(is.null(input_file)) return(NULL)
    m <- read_stars(input_file)
    if(is.null(m)) return(NULL)
    
    if(is(m[[1]], "factor")) {
      m <- map_factor_to_numeric(m)
    }
    return(m)  
  }
  
  getInputTable <- function(parameter, isfull = FALSE) {
    suppressWarnings({
      input_file <- getInputFile(parameter)
      # if(!file.exists(input_file)) return(NULL)
      if(is.null(input_file)) return(NULL)
      
      t <- NULL
      
      tryCatch({                     
          if(isfull) return(read.table(input_file))
          t <- read.table(input_file)[-1]
        },
        # Specifying error message
        error = function(e){         
          # print("There was an error message.")
        },
        
        warning = function(w){      
          # print("There was a warning message.")
        },
        
        finally = {            
          # print("finally Executed")
        }
      )
    })
    return(t)
  }
  
  is_boolean_map <- function(map) {
    ids_all <- unique(as.vector(map[[1]]))
    ids_all <- ids_all[!is.na(ids_all)]
    b <- c(0,1)
    if(all(ids_all %in% b)) return(T)
    return(F)
  }
  ############################################################
  ############################################################
  ############################################################
  
  #Land cover
  in_lc_df <- getInputTable("lcid[lctype]")
  names(in_lc_df) <- c("landcover", "lc_id")
  lc <- import_old_lc_par(in_lc_df)
  ptables <- generate_param_tables(lc)
  params <- append(params, ptables)
  lc_df <- params$landcover_df 
  
  #Color palette
  fpal <- getFilePath("lc.pal")
  if(!is.null(fpal)) {
    pal <- read.table(fpal)
    if(!is.null(pal)) {
      cpal <- rgb(pal/255)
      cpal_df <- data.frame(color = cpal, lc_id = c(0:(length(cpal)-1)))
      lc_df$color <- NULL
      lc_df <- merge(lc_df, cpal_df, by = "lc_id", all.x = T)
      params$landcover_df <- lc_df[lc_field]
    }
  }
  
  lu_df <- params$landuse_df
  flupal <- getFilePath("lu.pal")
  if(!is.null(flupal)) {
    rgbpal_df <- read.table(flupal)
    if(!is.null(rgbpal_df)) {
      lupal <- rgb(rgbpal_df/255)
      lupal_df <- data.frame(color = lupal, lu_id = c(1:(length(lupal))))
      lu_df$color <- NULL
      lu_df <- merge(lu_df, lupal_df, by = "lu_id", all.x = T)
      params$landuse_df <- lu_df[lu_field]
    }
  }
  
  if(!is.null(progress)) progress(20, "Uploading initlc")

  # TAKING INPUT MAPS ####################################################
  dim_map <- NA
  
  params$initlc_map <- getInputMap("initlc")
  if(is.null(params$initlc_map)) {
    errors <- c(errors, "initlc")
  } else {
    dim_map <- st_dimensions(params$initlc_map)
  }
  
  if(!is.null(progress)) progress(30, "Uploading general map")
  pfile_df <- params$pcraster_params
  map_data_df <- params$map_data_df
  mlist <- list()
  lapply(general_map_old_ids, function(x){
    f <- pfile_df[pfile_df$parameter == x, "value"]
    m_id <- paste0(x, "_map")
    mlist[[f]] <<- getInputMap(x)
    if(is.null(mlist[[f]])) {
      errors <<- c(errors, f)
    } else {
      st_dimensions(mlist[[f]]) <<- dim_map
      map_data_df[map_data_df$id == m_id, "file"] <<- f
    }
  })
  
  if(!is.null(progress)) progress(40, "Uploading suitability map")
  apply(suit_map_df, 1, function(x){
    f <- pfile_df[pfile_df$parameter == x["old_id"], "value"]
    m_id <- x["id"]
    mlist[[f]] <<- getInputMap(x["old_id"])
    if(is.null(mlist[[f]])) {
      errors <<- c(errors, f)
    } else {
      st_dimensions(mlist[[f]]) <<- dim_map
      map_data_df[map_data_df$id == x["new_id"], "file"] <<- f
    }
  })
  
  if(!is.null(progress)) progress(50, "Uploading proximity map")
  apply(dist_map_df, 1, function(x){
    f <- pfile_df[pfile_df$parameter == x["fid"], "value"]
    m_id <- x["map_type"]
    mlist[[f]] <<- getInputMap(x["fid"])
    if(is.null(mlist[[f]])) {
      errors <<- c(errors, f)
    } else {
      st_dimensions(mlist[[f]]) <<- dim_map
      map_data_df[map_data_df$id == x["new_id"], "file"] <<- f
    }
  })
  params$map_list <- mlist
  params$map_data_df <- map_data_df
  
  ######################################################## 
  if(!is.null(progress)) progress(60, "Uploading scalar data")
  
  # # DISASTER #########
  disastersocialimpact_df <- getInputTable("disastersocialimpact[socialdisaster]")
  if(is(disastersocialimpact_df, "data.frame")) {
    names(disastersocialimpact_df) <- c("var", "value")
  }

  other_df <- getInputTable("other[free]")
  if(is(other_df, "data.frame"))
    in_disaster_time <- other_df$V3[other_df$V2 == "disaster_time"]
  
  #INITIALISATION OF LANDCOVER AND LANDUSE
  #LANDCOVER IDS
  lc_df <- data.frame(
    lc_id = c(0:40),
    # luid = c(0, rep(1,4), 2:5, rep(6,4), rep(7,4), rep(8,4), rep(9,4), rep(10,4),
    #          rep(11,4), rep(12,4), rep(13,4)),
    lctype = c("set", "for_pion", "for_ysec", "for_osec", "for_prim", "agr1",
               "agr2",      "agr3",      "agr4",      "af1_pion",  "af1_eprod", "af1_lprod",
               "af1_pprod", "af2_pion",  "af2_eprod","af2_lprod", "af2_pprod", "af3_pion",
               "af3_eprod", "af3_lprod", "af3_pprod", "af4_pion",  "af4_eprod", "af4_lprod",
               "af4_pprod", "af5_pion",  "af5_eprod", "af5_lprod", "af5_pprod", "af6_pion",
               "af6_eprod", "af6_lprod", "af6_pprod", "af7_pion",  "af7_eprod", "af7_lprod",
               "af7_pprod", "af8_pion",  "af8_eprod", "af8_lprod", "af8_pprod")
  )
  #LANDUSE IDS
  lu_df <- data.frame(
    lu_id = c(1:14),
    lutype = c("SET", "FOR", "AGR1", "AGR2", "AGR3", "AGR4", "AF1", "AF2", "AF3",
               "AF4", "AF5", "AF6", "AF7", "AF8")
  )
  # 
  # lc_df <- merge(lc_df, lu_df, by = "luid")
  # 
  livelihood_df <- data.frame(
    ll_id = c(1:15),
    livelihoodtype = c("offfarm", "ntfp", "timber", "food1", "food2", "food3",
                        "food4", "af1", "af2", "af3", "af4", "af5", "af6", "af7", "af8")
  )

  ###################################################
  # land cover, land cover and livelihood parameters initialization 
    
  parbio1_df <- lc_df
  parbio2_df <- livelihood_df 
  pareco1_df <- livelihood_df
  parsos_df <- livelihood_df
  ###################################################
  # BIOPHYSICAL PARAMETERS
  
  # Biophysical 1 ############
  
  lctimebound_df <- getInputTable("lctimebound[lctype]")
  names(lctimebound_df) <- c("lctype", "lctimebound.min")
  #modify lctimebound range 
  maxv <- .Machine$integer.max
  lctimebound_df$lctimebound.max <- lctimebound_df$lctimebound.min 
  lctimebound_df$lctimebound.max[startsWith(lctimebound_df$lctype, "agr")] <- maxv
  lctimebound_df$lctimebound.max[endsWith(lctimebound_df$lctype, "_pion")] <- maxv
  lctimebound_df$lctimebound.max <- c(lctimebound_df$lctimebound.max[-1], rep(maxv, 1))
  parbio1_df <- merge(parbio1_df, lctimebound_df, by = "lctype")
  
  initlcagestat_df <- getInputTable("initlcagestat[lctype][stat]")
  initlcagestat_df <- reshape(initlcagestat_df, direction = "wide", idvar = "V2", timevar = "V3")
  names(initlcagestat_df) = c("lctype","initlcage.mean","initlcage.cv")
  parbio1_df <- merge(parbio1_df, initlcagestat_df, by = "lctype")
  
  # SOIL FERTILITY
  soilstat_df <- getInputTable("soilstat[lctype][soilfertilityproperties][stat]")
  soilstat_df <- reshape(soilstat_df[soilstat_df[3] != "cv",][c(1,2,4)], 
                         direction = "wide", idvar = c("V2"), timevar = "V3")
  names(soilstat_df) <- c("lctype", "depletionrate", "halftimerecovery")
  parbio1_df <- merge(parbio1_df, soilstat_df, by = "lctype")

  # LANDCOVER PROPERTY
  lcprostat_df <- getInputTable("lcprostat[lctype][lcproperties][stat]")
  lcprostat_df <- reshape(lcprostat_df, direction = "wide", idvar = c("V2", "V3"), 
                          timevar = "V4")
  lcprostat_df <- reshape(lcprostat_df, direction = "wide", idvar = c("V2"), 
                          timevar = "V3")
  names(lcprostat_df) <- c("lctype", "agb.mean", "agb.cv", 
                           "floorbiomassfrac.mean", "floorbiomassfrac.cv",
                           "pfirespread.mean", "pfirespread.cv")
  parbio1_df <- merge(parbio1_df, lcprostat_df, by = "lctype")

  getLivelihoodStat <- function(parameter) {
    stat_df <- getInputTable(parameter)
    stat_df <- reshape(stat_df, direction = "wide", idvar = "V2", 
                       timevar = "V3")
    names(stat_df) <- c("livelihoodtype", "mean", "cv")
    return(stat_df)
  }
  
  # Biophysical 2 ############
  
  harvestingstat_df <- getLivelihoodStat("harvestingstat[livelihoodtype][stat]")
  names(harvestingstat_df) <- c("livelihoodtype", "harvestingstat.mean", "harvestingstat.cv")
  parbio2_df <- merge(parbio2_df, harvestingstat_df, by = "livelihoodtype")
  
  storeprop_df <- getInputTable("storeprop[storeproperties][livelihoodtype]")
  storeprop_df <- reshape(storeprop_df, direction = "wide", idvar = "V3", timevar = "V2")
  names(storeprop_df) <- c("livelihoodtype", "demandpercapita", "ptosell", "lossfrac")
  parbio2_df <- merge(parbio2_df, storeprop_df, by = "livelihoodtype")
  
  spatialw_df <- getInputTable("spatialw[expansiondeterminant][livelihoodtype]")
  names(spatialw_df) <-  c("expansiondeterminant", "livelihoodtype", "value")
  spatialw_df2 <- reshape(spatialw_df, direction = "wide", idvar = "livelihoodtype",
                          timevar = "expansiondeterminant")
  names(spatialw_df2) <- sub("value.","",names(spatialw_df2))
  parbio2_df <- merge(parbio2_df, spatialw_df2, by = "livelihoodtype")
  
  if(!is.null(progress)) progress(70)
  #################################
  # ECONOMIC PARAMETERS
  
  # Economic 1 ############
  pricestat_df <- getLivelihoodStat("pricestat[livelihoodtype][stat]")
  names(pricestat_df) <- c("livelihoodtype", "pricestat.mean", "pricestat.cv")
  pareco1_df <- merge(pareco1_df, pricestat_df, by = "livelihoodtype")
  
  initknowledge_df <- getInputTable("initknowledge[knowledgetype][livelihoodtype][agenttype]")
  initknowledge_df <- reshape(initknowledge_df, direction = "wide", idvar = c("V3", "V4"), timevar = "V2")
  names(initknowledge_df) = c("livelihoodtype", "agent","exppayofftolabor","exppayofftoland")
  initknowledge_df <- reshape(initknowledge_df, direction = "wide", idvar = c("livelihoodtype"), timevar = "agent")
  pareco1_df <- merge(pareco1_df, initknowledge_df, by = "livelihoodtype")

  subsidy_df <- getInputTable("subsidy[livelihoodtype][subsidytype]")
  names(subsidy_df) <- c("livelihoodtype", "subsidytype", "val")
  subsidy_df2 <- reshape(subsidy_df, direction = "wide", idvar = "livelihoodtype",
                         timevar = "subsidytype")
  names(subsidy_df2) <- c("livelihoodtype", "subsidy_estsub", "subsidy_mntsub")
  pareco1_df <- merge(pareco1_df, subsidy_df2, by = "livelihoodtype")
  
  estcoststat_df <- getLivelihoodStat("estcoststat[livelihoodtype][stat]")
  names(estcoststat_df) <- c("livelihoodtype", "estcoststat.mean", "estcoststat.cv")
  pareco1_df <- merge(pareco1_df, estcoststat_df, by = "livelihoodtype")
  
  estlaborstat_df <- getLivelihoodStat("estlaborstat[livelihoodtype][stat]")
  names(estlaborstat_df) <- c("livelihoodtype", "estlaborstat.mean", "estlaborstat.cv")
  pareco1_df <- merge(pareco1_df, estlaborstat_df, by = "livelihoodtype")
  
  extlaborstat_df <- getLivelihoodStat("extlaborstat[livelihoodtype][stat]")
  names(extlaborstat_df) <- c("livelihoodtype", "extlaborstat.mean", "extlaborstat.cv")
  pareco1_df <- merge(pareco1_df, extlaborstat_df, by = "livelihoodtype")
  
  ###############################################################
  # Economic 2 ############
  
  nonlaborcoststat_df <- getInputTable("nonlaborcoststat[lctype][livelihoodtype][stat]")
  nonlaborcoststat_df <- reshape(nonlaborcoststat_df, direction = "wide", idvar = c("V2","V3"), 
                                 timevar = "V4")
  names(nonlaborcoststat_df) <- c("lctype", "livelihoodtype", "mean", "cv")
  nonlaborcoststat_df <- merge(nonlaborcoststat_df, lc_df[c("lctype", "lc_id")], by = "lctype")
  
  yieldstat_df <- getInputTable("yieldstat[lctype][livelihoodtype][stat]")
  yieldstat_df <- reshape(yieldstat_df, direction = "wide", idvar = c("V2","V3"), 
                          timevar = "V4")
  names(yieldstat_df) <- c("lctype", "livelihoodtype", "mean", "cv")
  yieldstat_df <- merge(yieldstat_df, lc_df[c("lctype", "lc_id")], by = "lctype")
  
  #############################################################
  # SOCIO-CULTURAL PARAMETERS
   
  culturaldeliberation_df <- getInputTable("culturaldeliberation[livelihoodtype]")
  names(culturaldeliberation_df) <- c("livelihoodtype", "culturaldeliberation")
  parsos_df <- merge(parsos_df, culturaldeliberation_df, by = "livelihoodtype")

  extensionprop_df <- getInputTable("extensionprop[extensionproperties][livelihoodtype]")
  extensionprop_df <- reshape(extensionprop_df, direction = "wide", idvar = c("V3"), 
                              timevar = "V2", varying = unique(extensionprop_df$V2))
  colnames(extensionprop_df)[1] <- c("livelihoodtype")
  parsos_df <- merge(parsos_df, extensionprop_df, by = "livelihoodtype")

  extensionsuggestion_df <- getInputTable("extensionsuggestion[knowledgetype][livelihoodtype]")
  extensionsuggestion_df <- reshape(extensionsuggestion_df, direction = "wide", idvar = c("V3"), 
                                    timevar = "V2", varying = unique(extensionsuggestion_df$V2))
  colnames(extensionsuggestion_df)[1] <- c("livelihoodtype")
  parsos_df <- merge(parsos_df, extensionsuggestion_df, by = "livelihoodtype")

  pfireuse_df <- getInputTable("pfireuse[livelihoodtype]")
  names(pfireuse_df) <-  c("livelihoodtype", "pfireuse")
  parsos_df <- merge(parsos_df, pfireuse_df, by = "livelihoodtype")
  
  ###################################
  # FARMER LEARNING
  in_agentprop_df <- getInputTable("agentprop[agenttype][agentproperties]")
  proplabel <- unique(in_agentprop_df$V3)
  in_agentprop_df <- reshape(in_agentprop_df, direction = "wide", idvar = "V2", 
                          timevar = "V3")
  names(in_agentprop_df) <- c("agent", proplabel)

  # DEMOGRAPHY
  in_demographics_df <- getInputTable("demographics[demographicalproperties]")
  names(in_demographics_df) <- c("var", "value")

  in_unitconverter_df <- getInputTable("unitconverter[converter]")
  if(!is.null(in_unitconverter_df)) {
    names(in_unitconverter_df) <-  c("converter", "value")
  } 
  
  if(!is.null(progress)) progress(80)
  ##########################################################################
  #Scalar input
  lc_df <- params$landcover_df
  pb1 <- parbio1_df
  pb1["lctype"] <- NULL
  pb1 <- merge(pb1, lc_df[c("lc_id", "lc_short")], by = "lc_id")
  params$bio_lc_df <- pb1
  
  ll_df <- params$livelihood_df
  pb2 <- parbio2_df
  pb2["livelihoodtype"] <- NULL
  pb2 <- merge(pb2, ll_df[c("ll_id", "ll_short")], by = "ll_id")
  params$bio_ll_df <- pb2
  
  pe1 <- pareco1_df
  pe1["livelihoodtype"] <- NULL
  pe1 <- merge(pe1, ll_df[c("ll_id", "ll_short")], by = "ll_id")
  params$eco_ll_df <- pe1
  
  ps <- parsos_df
  ps["livelihoodtype"] <- NULL
  ps <- merge(ps, ll_df[c("ll_id", "ll_short")], by = "ll_id")
  params$soc_ll_df <- ps
  
  nlc <- nonlaborcoststat_df[nonlaborcoststat_df$mean > 0,]
  nlc <- nlc[c("lc_id", "mean", "cv")]
  names(nlc) <- c("lc_id", "nonlaborcost.mean", "nonlaborcost.cv")
  lc_df_id <- lc_df[c("lc_id", "lc_short")]
  pe2 <- merge(lc_df_id, nlc, by = "lc_id", all.x = T)
  ys <- yieldstat_df[yieldstat_df$mean > 0,]
  ys <- ys[c("lc_id", "mean", "cv")]
  names(ys) <- c("lc_id", "yield.mean", "yield.cv")
  pe2 <- merge(pe2, ys, by = "lc_id", all.x = T)
  pe2[is.na(pe2)] <- 0
  params$eco_lc_df <- pe2
  
  params$demographics_df <- demographics_df
  params$agentprop_df <- agentprop_df
  params$disaster_df <- disaster_df
  params$converter_df <- converter_df
  
  params$demographics_df$value <- in_demographics_df$value
  params$agentprop_df$value1 <- unlist(in_agentprop_df[1,2:5])
  params$agentprop_df$value2 <- unlist(in_agentprop_df[2,2:5])
  params$disaster_df[4, "value"] <- in_disaster_time
  if(!is.null(disastersocialimpact_df)) {
    params$disaster_df[1:3, "value"] <- disastersocialimpact_df$value
  }
  if(!is.null(in_unitconverter_df)) {
    params$converter_df$value <- in_unitconverter_df$value
  }

  if(!is.null(progress)) progress(100, "Parameters upload completed")
  return(params)
}

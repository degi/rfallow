library(stars)
library(reshape) 
library(dplyr)
source("RFallow_funcs.R")

##### load FALLOW parameters into data frame table
runRFallow <- function(params, 
                       sim_time = 0, 
                       progress_iteration = NULL,
                       progress_detail = NULL) {
  
  start_time <- Sys.time()
  
  lc_df <- params$landcover_df
  lu_df <- params$landuse_df
  ll_df <- params$livelihood_df

  predictStat <- function(mean, cv) {
    mean + mean * cv * rnorm(length(mean))
  }
  
  getVarValue <- function(df, var_name) {
    return(df$value[df$field == var_name])
  }
  
  #SAVING OUTPUT MAPS AND TIME SERIES
  initlc_map <- params[["initlc_map"]]
  
  out_map <- initlc_map
  names(out_map) <- "LC 0"
  
  # #output maps
  out_lumap <- list()
  out_lcmap <- list()
  out_soilfert <- list()
  out_agbiomass <- list()
  out_agcarbon <- list()
  out_fire <- list()
  out_age <- list()

  get_map <- function(id_map) {
    df <- params$map_data_df
    f <- df[df$id == id_map, "file"]
    if(is.null(f)) return(NULL)
    if(is.na(f)) return(NULL)
    return(params$map_list[[f]])
  }
  
  #INITIALISATION
  # area_map <- params[["area_map"]]
  area_map <- params$initlc_map
  area_map[!is.na(area_map)] <- T
  
  allnewplots_map <- !area_map
  logzone_map <- get_map("initlog_map")
  ntfpzone_map <- !area_map
  
  demographics_df <- params$demographics_df
  totpop <- demographics_df$value[demographics_df$field == "initpop"]
  totfinance <- demographics_df$value[demographics_df$field == "initfinance"]
  soilfert_map <- get_map("initsoilfert_map")
  marginalagriculture_map <- area_map
  marginalagriculture_map[area_map == 1] <- NA
  marginalAF_map <- area_map
  marginalAF_map[area_map == 1] <- NA
  agbiomass <- 0;
  totlaborcosts <- 0;
  totnonlaborcosts <- 0;
  totestcost <- 0;
  
  parbio1_df <- params$bio_lc_df
  parbio1_df <- merge(parbio1_df, params$landcover_df[c("lc_id", "lu_id", "landuse")], by = "lc_id")
  parbio2_df <- params$bio_ll_df
  pareco1_df <- params$eco_ll_df
  
  yieldstat_df <- params$eco_lc_df[c("lc_id", "yield.mean", "yield.cv")]
  yieldstat_df <- merge(yieldstat_df, lc_df[c("lc_id", "ll_id")], by = "lc_id")
  
  nonlaborcoststat_df <- params$eco_lc_df[c("lc_id", "nonlaborcost.mean", "nonlaborcost.cv")]
  nonlaborcoststat_df <- merge(nonlaborcoststat_df, lc_df[c("lc_id", "ll_id")], by = "lc_id")
  
  pfireuse_df <- params$soc_ll_df[c("ll_id", "pfireuse")]
  
  extensionprop_df <- params$soc_ll_df[c("ll_id", "event", "credibility", "exposure")]
  extensionsuggestion_df <- params$soc_ll_df[c("ll_id", "explabor", "expland")]
  
  getSpatialW <- function(expansiondeterminant, ll_id) {
    parbio2_df[parbio2_df$ll_id == ll_id, expansiondeterminant]
  }

  #INITIALISATION OF LANDCOVER AGE
  
  lcage_map <- rnorm_by_ids(initlc_map, parbio1_df$lc_id, 
                            parbio1_df$initlcage.mean, 
                            parbio1_df$initlcage.cv)
  
  lu_map = initlc_map
  names(lu_map) = "lu"
  lu_map <- reclassify_map(initlc_map, lc_df[c("lc_id", "lu_id")])
  
  #INITIALISATION OF RETURN TO LAND AND TO LABOR
  
  initknowledge_df <- data.frame(ll_id = pareco1_df$ll_id, 
                                 exppayofftolabor = pareco1_df$exppayofftolabor.agent1,
                                 exppayofftoland = pareco1_df$exppayofftoland.agent1,
                                 agent = "agent1")
  
  initknowledge_df <- rbind(initknowledge_df, 
                            data.frame(ll_id = pareco1_df$ll_id, 
                                       exppayofftolabor = pareco1_df$exppayofftolabor.agent1,
                                       exppayofftoland = pareco1_df$exppayofftoland.agent2,
                                       agent = "agent2"))
  
  #INITIALISATION OF STORAGE (ASSUMED ENOUGH FOOD AT THE INITIAL SIMULATION YEAR)
  
  store_df <- parbio2_df[c("ll_id", "lossfrac", "demandpercapita", "ptosell")] 
  store_df$store <- totpop * store_df$demandpercapita
  
  #STANDARISATION OF SOME VARIABLES FOR DETERMINING THE MOST DOMINANT FACTOR
  #AFFECTING PLOT EXPANSION 
  
  #to standardize slope to road, to river, to market, to settlement 
  #and to processing industry (0-1)
  
  zslope_map <- standardize_map(get_map("slope_map")) 

  #PRESET
  #POTENTIAL HARVESTING ZONES AND AREAS FOR EACH LIVELIHOOD OPTION
  
  get_lcid_by_llid <- function(ll_id) {
    lc_df <- params$landcover_df
    if(ll_id == NTFP || ll_id == TIMBER)
      return(lc_df[lc_df$landuse == "Forest", "lc_id"])
    return(lc_df[!is.na(lc_df$ll_id) & lc_df$ll_id == ll_id, "lc_id"])
  }
  
  reserve_map <- get_map("reserve_map")
  
  not_reserve_map <- !reserve_map
  phzonecat_map_list <- list()
  llids <- params$livelihood_df$ll_id
  phzonecat_map_list <- vector("list", length(llids))
  names(phzonecat_map_list) <- llids
  phzonecat_map_list <-lapply(phzonecat_map_list, \(i) not_reserve_map)
  
  phzonecat_map_list[[OFFFARM]] <- !area_map
  phzonecat_map_list[[NTFP]] <- not_reserve_map & ntfpzone_map
  if(!is.null(logzone_map))
    phzonecat_map_list[[TIMBER]] <- not_reserve_map & logzone_map
  
  mdist_df <- with(params, map_data_df[map_data_df$group == "proximity",])
  
  # map_data 
  
  zmap <- apply(mdist_df, 1, fmap = params$map_list, FUN = function(df, fmap){
    standardize_map(fmap[[df["file"]]])
  }) 
  if(is.null(zmap)) {
    print("ERROR: proximity map is NULL!")
    return()
  }
  names(zmap) <- mdist_df$file

  getMapll <- function(ll_id) {
    f <- mdist_df[mdist_df$ll_id == ll_id & !is.na(mdist_df$ll_id), "file"]
    if(length(f) == 0) return(NULL)
    zmap[[f]]
  }
  
  getProximityMap <- function(label) {
    f <- mdist_df[mdist_df$label == label, "file"]
    if(length(f) == 0) return(NULL)
    zmap[[f]]
  }
  
  # suitability map
  get_suitability_map <- function(ll_id){
    msuit_df <- with(params, map_data_df[map_data_df$group == "suitability",])
    f <- msuit_df[msuit_df$ll_id == ll_id, "file"]
    if(length(f) == 0) return(NULL)
    return(params$map_list[[f]])
  } 
  
  adf <- params$agentprop_df[c("field", "value1", "value2")]
  names(adf) <- c("field", "agent1", "agent2")
  melt_adf <-  melt(adf, id = c("field")) 
  n <- unique(melt_adf$field)
  agentprop_df <- reshape(melt_adf, direction = "wide", idvar = "variable", timevar = "field", varying = n)
  names(agentprop_df)[1] <- "agent"

  ###########################################
  out_lc_df <- NULL
  out_ll_df <- NULL
  out_val_df <- NULL
  out_map <- list()
  
  
  # tssconsole <- params$tssconsole
  tssconsole <- F
  ###########################################################################
  #simulation time
  # simend <- 30
  pixelsize <- 1
  ############################################################################
  # totnetincome <- max(0, totselling-totbuying-totnonlaborcosts-totlaborcosts-totestcost
  
  totnetincome_df <- data.frame()
  
  disaster_time <-  with(params, disaster_df[disaster_df$field == "disaster_time", "value"])
  #### SIMULATION LOOP HERE
  # time <- 1
  for(time in 1:sim_time) {
    print(paste("######### time:", time, " ##############"))
    te <- Sys.time() - start_time
    ie <- time - 1
    estt <- 0 
    if(ie > 0) estt <- round((te/ie)*(sim_time-time), 2) 
    if(!is.null(progress_iteration)) {
      progress_iteration(
        time, paste0("Iteration [estimated time: ", format(estt),"]"))
    }
    if(!is.null(progress_detail)) progress_detail(1, "Start iteration")
    
    balance <- totfinance
    totbuying  <- 0
    totselling  <- 0
  
    #DISASTER EVENT AND IMPACT
    disasterimpactonhuman <- 0
    disasterimpactonmoney <- 0
    disasterimpactonworkingday <- 0
    disasterimpactedzone_map <- !area_map
    if(time == disaster_time) {
      #TODO: to be updated
      disasterimpactonhuman <- getVarValue(disastersocialimpact_df, "human")
      disasterimpactonmoney <- getVarValue(disastersocialimpact_df, "money")
      disasterimpactonworkingday <- getVarValue(disastersocialimpact_df, "workingday")
      disasterimpactedzone_map <- disaster_map
    } 
    
    #DEFINING ROAD, RIVER, MARKET, AND PROCESSING INDUSTRY MAPS 
    #FOR EACH SIMULATE TIME INTERVAL
    
    # map_data <- NULL
    # #TODO: what if dynamicmap_df is null? no data?
    # if(!is.null(dynamicmap_df)) {
    #   if(isOnPeriod(time, "period1")) {
    #     map_data <- params$map_data_a
    #   } else if(isOnPeriod(time, "period2")) {
    #     map_data <- params$map_data_b
    #   } else if(isOnPeriod(time, "period3")) {
    #     map_data <- params$map_data_c
    #   } else {
    #     map_data <- params$map_data_d
    #   }
    # } else {
    #   map_data <- params$map_data_a
    # }
    #TO READ VALUES FROM TIMESERIES VARIABLES
    
    #to get values for extension availability for each livelihood option
    sub_df <- NULL
    ex_df <- NULL
    # ex_df <- params$ex_ts_df[c("livelihoodtype", time)]
    # if(!is.null(ex_df))
    #   names(ex_df) <- c("livelihoodtype","ex")
    # #to get values for subsidy availability for each livelihood option
    # sub_df <- params$sub_ts_df[c("livelihoodtype", time)]
    # if(!is.null(sub_df))
    #   names(sub_df) <- c("livelihoodtype","sub")
    
    # if(params$tssconsole) {
    #   #to get values for price dynamic for each livelihood option
    #   price_df <- price_ts_df[c("livelihoodtype", time)] 
    #   names(price_df) <- c("livelihoodtype", "price")
    # } else {
      #TO GENERATE PRICE IF NOT SIMULATED IN TIME SERIES
      # price_df <- predictStatValue(pricestat_df, "price")
      price_df <- pareco1_df["ll_id"]
      price_df$price <- predictStat(pareco1_df$pricestat.mean, pareco1_df$pricestat.cv)
    # }
  
    #TO GENERATE HARVESTING PRODUCTIVITY
    
    # harvestingefficiency_df <- predictStatValue(harvestingstat_df, "harvestingefficiency")
    harvestingefficiency_df <- parbio2_df["ll_id"]
    harvestingefficiency_df$harvestingefficiency <- predictStat(
      parbio2_df$harvestingstat.mean, parbio2_df$harvestingstat.cv
    )
    
    #CAPITAL (COST, LABOR AND EXTERNAL LABOR) REQUIREMENT FOR PLOT ESTABLISHMENT
    
    # estcost_df <- predictStatValue(estcoststat_df, "estcost")
    estcost_df <- pareco1_df["ll_id"]
    estcost_df$estcost <- predictStat(pareco1_df$estcoststat.mean, 
                                      pareco1_df$estcoststat.cv)
    estcost_df$estcost <- pmax(0, estcost_df$estcost)
    
    # estlabor_df <- predictStatValue(estlaborstat_df, "estlabor")
    estlabor_df <- pareco1_df["ll_id"]
    estlabor_df$estlabor <- predictStat(pareco1_df$estlaborstat.mean, 
                                      pareco1_df$estlaborstat.cv)
    estlabor_df$estlabor <- pmax(0, estlabor_df$estlabor)
    
    # extlabor_df <- predictStatValue(extlaborstat_df, "extlabor")
    extlabor_df <- pareco1_df["ll_id"]
    extlabor_df$extlabor <- predictStat(pareco1_df$extlaborstat.mean, 
                                        pareco1_df$extlaborstat.cv)
    extlabor_df$extlabor <- pmax(0, extlabor_df$extlabor)
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(5, "Calculate labor capital")
    #####################################################################
    
    #TO CALCULATE TOTAL LABOR (LABOR CAPITAL)
    # agentprop_df <-  params$agentprop_df
    tot_labor_df <- data.frame (agent = agentprop_df$agent)
    tot_labor_df$tot_labor <- totpop * agentprop_df$popfraction * 
      getVarValue(demographics_df, "laborfraction") *
      getVarValue(demographics_df, "workingdays") *
      (1 - disasterimpactonworkingday/100)
    
    #CALCULATING FRACTION AND AVAILABLE LABOR FOR EACH LIVELIHOOD OPTION 
    #BASED ON RETURN TO LABOR, CULTURAL DELIBERATION, AND PRIORITIZATION DEGREE
    #NOT ALL LABOR ALLOCATIONS ARE BASED ON ECONOMIC PROPENSITY ONLY
    print("#CALCULATING FRACTION AND AVAILABLE LABOR FOR EACH LIVELIHOOD OPTION")
    
    ### the table should be reset on each loop
    exppayoff_df <- initknowledge_df
    # culturaldeliberation_df <- params$culturaldeliberation_df
    culturaldeliberation_df <- params$soc_ll_df[c("ll_id", "culturaldeliberation")]
    s <- merge(exppayoff_df, culturaldeliberation_df, by = "ll_id") 
    s <- merge(s, agentprop_df[c("agent", "prioritization")], by = "agent")
    s$frac_tlab <- s$culturaldeliberation * pmax(0, s$exppayofftolabor) ^ s$prioritization
    
    sum_by_agent <- aggregate(s$frac_tlab, by=list(Category=s$agent), FUN=sum)
    names(sum_by_agent)<- c("agent", "sum_lab")
    count_by_agent <- aggregate(s$frac_tlab, by=list(Category=s$agent), FUN=length)
    sum_by_agent$count_lab <- count_by_agent$x
    s <- merge(s, sum_by_agent, by = "agent")
    s$labormoneyfrac <- ifelse(s$sum_lab > 0, s$frac_tlab/s$sum_lab,  1/s$count_lab) 
    labormoneyfrac_df <- s[c("agent", "ll_id", "labormoneyfrac")]
    
    #AVAILABLE LABOR FOR EACH LIVELIHOOD OPTION
    
    labormoneyfrac_df <-  merge(labormoneyfrac_df, tot_labor_df, by = "agent")
    labormoneyfrac_df$availablelabor_int <- labormoneyfrac_df$labormoneyfrac *
      labormoneyfrac_df$tot_labor
    availablelabor_df <- aggregate(labormoneyfrac_df$availablelabor_int, 
                                   by=list(Category=labormoneyfrac_df$ll_id), FUN=sum)
    names(availablelabor_df) <- c("ll_id", "availablelabor_int")
    availablelabor_df <- merge(availablelabor_df, extlabor_df, by = "ll_id")
    availablelabor_df$availablelabor <- availablelabor_df$availablelabor_int +
      availablelabor_df$extlabor
    
    #CALCULATING FRACTION AND AVAILABLE LAND FOR EACH LIVELIHOOD OPTION 
    #BASED ON RETURN TO LAND, CULTURAL DELIBERATION, AND PRIORITIZATION DEGREE
    #NOT ALL LAND ALLOCATIONS ARE BASED ON ECONOMIC PROPENSITY ONLY
    print("#CALCULATING FRACTION AND AVAILABLE LAND FOR EACH LIVELIHOOD OPTION ")
  
    s$frac_tland <- s$culturaldeliberation * pmax(0, s$exppayofftoland) ^ s$prioritization
    sum_by_agent <- aggregate(s$frac_tland, by=list(Category=s$agent), FUN=sum)
    names(sum_by_agent)<- c("agent", "sum_land")
    count_by_agent <- aggregate(s$frac_tland, by=list(Category=s$agent), FUN=length)
    sum_by_agent$count_land <- count_by_agent$x
    s <- merge(s, sum_by_agent, by = "agent")
    s$landfrac <- ifelse(s$sum_land > 0, s$frac_tland/s$sum_land,  1/s$count_land) 
    #off farm
    s$landfrac[s$ll_id == OFFFARM] <- 0
    landfrac_df <- s[c("agent", "ll_id", "landfrac")]
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(10, "Updating landcover map by plot age")
    #####################################################################
    
    #TO CHECK!!
    
    #UPDATING LANDCOVER MAP BASED ON CURRENT PLOT AGE AND LANDUSE MAP. LANDUSE
    #MAP IS UPDATED LATER BELOW 
    print("#UPDATING LANDCOVER MAP BASED ON CURRENT PLOT AGE AND LANDUSE MAP. LANDUSE")
    lc_map <- lu_map
    lc_map[!is.na(lu_map)] <- -1
    for(i in 1:nrow(parbio1_df)) {
      lc <- parbio1_df[i,]
      lc_map[lu_map == lc$lu_id & lcage_map >= lc$lctimebound.min & 
               lcage_map < lc$lctimebound.max] <- lc$lc_id
    }
    lccount_df <- data.frame(table(lc_map))
    names(lccount_df) <- c("lc_id", "area")
    lcarea_df <- merge(lc_df[c("lu_id", "lc_id")], lccount_df, by = "lc_id")
    # TODO: ini cuma buat output -> gak perlu dihitung sekarang
    
    #report lcmap
    
    #THE AREA OF EACH LANDUSE TYPE
    luarea_df <- aggregate(lcarea_df$area, by=list(Category=lcarea_df$lu_id), FUN=sum)
    # TODO: ini cuma buat output -> gak perlu dihitung sekarang
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(15, "Calculate subcathment area")
    #####################################################################
    
    #CALCULATING LANDCOVER AREA IN EACH SUBCATCHMENT AREA
    if(is(params$subcatchment_map, "stars")) {
      lcsc_map <- c(lc_map, params$subcatchment_map)
      lcsc_df <- data.frame(table(lcsc_map))
      names(lcsc_df) <- c("lc_id", "scid", "area")
    } else {
      lcsc_df <- data.frame(table(lc_map))
      names(lcsc_df) <- c("lc_id", "area")
      lcsc_df$scid <- 0
    }
    #AREA OF EACH SUBCATCHMENT
    scareatot_df <- aggregate(lcsc_df$area, by=list(Category=lcsc_df$scid), FUN=sum)
    names(scareatot_df) <- c("scid", "sc_total_area")
    
    #FRACTION OF EACH LANDCOVER TYPE IN A SUBCATCHMENT
    lcsc_df <-  merge(lcsc_df, scareatot_df, by = "scid")
    lcsc_df$sclcareafrac <- lcsc_df$area/lcsc_df$sc_total_area
    
    #LAND CAPITAL: MAP DESCRIBING AREAS POSSIBLE FOR CONVERSION WHICH ARE UNPROTECTED FORESTS 
    #AND UNPRODUCTIVE AGRICULTURAL PLOTS
    lcids <- with(lc_df, lc_id[landuse == "Forest" |
            (landuse == "Tree-based system" & growth_stage == "Old")])
    
    criticalzone_map <- area_map
    criticalzone_map[!is.na(criticalzone_map)] <- 0
    criticalzone_map[(lc_map %in% lcids | marginalagriculture_map | 
                        marginalAF_map) & !reserve_map ] <- 1
   
    totcritzonearea <- sum(criticalzone_map[[1]], na.rm = TRUE)
    
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(20, "Calculate new plot potential")
    #####################################################################
    
    #CALCULATING AREAS OF POTENTIAL NEW PLOT FOR EACH LIVELIHOOD OPTION 
    #BASED ON THE ABOVE FRACTION OF RETURN TO LAND  
    #weighted return to land (landfrac) from two agent types
    print("#CALCULATING AREAS OF POTENTIAL NEW PLOT FOR EACH LIVELIHOOD OPTION")
    
    landfrac_df <- merge(landfrac_df, agentprop_df[c("agent", "popfraction")], 
                         by = "agent")
    landfrac_df$ag_landfrac <- landfrac_df$landfrac * landfrac_df$popfraction
    
    critzonearea_df <- aggregate(landfrac_df$ag_landfrac, 
                                 by=list(Category=landfrac_df$ll_id),
                                 FUN=sum)
    names(critzonearea_df) <- c("ll_id", "critzonearea")
    critzonearea_df$critzonearea <- critzonearea_df$critzonearea * totcritzonearea
    critzonearea_df$critzonearea[critzonearea_df$ll_id == OFFFARM] <- 0
    
    
    #THE CRITICAL ZONE IS FOR LIVELIHOOD A IF THE RANDOM NUMBER IN THE ZONE IS LOWER
    #THAN THE LAND FRACTION FOR THE LIVELIHOOD A AND SUITABLE FOR THE LIVELIHOOD A
    #FOR EXAMPLE, IF THERE ARE 3 OPTIONS WITH PROBABILITY 0.3, 0.4, AND 0.3 THEN 
    #THE CRITICALZONE WITH RANDOM NUMBERS OF 0-0.3 BELONG TO 1ST OPTION, 0.3-0.7 
    #FOR 2ND OPTION, AND 0.7-1 FOR 3RD OPTION 
    
    #generating random numbers in critical zone
    critzone_map_list <- list()
    
    if(totcritzonearea > 0) { 
      
      randcritzone_map <- runif_to_map(criticalzone_map)
      randcritzone_map[criticalzone_map == 0] <- NA
    
      critzoneprob_df <- critzonearea_df[critzonearea_df$critzonearea != 0,]
      critzoneprob_df$critzoneprob <- critzoneprob_df$critzonearea/totcritzonearea
      critzoneprob_df$probmax <- cumsum(critzoneprob_df$critzoneprob)
      critzoneprob_df$probmin <- c(0, head(critzoneprob_df$probmax, -1))
     
      #map for critical zone of each livelihood option
      for(i in 1:nrow(critzoneprob_df)) {
        cp <- critzoneprob_df[i,]
        suit_map <- get_suitability_map(cp$ll_id)
        if(is.null(suit_map)) {
          m <- !area_map
          m[randcritzone_map > cp$probmin & 
              randcritzone_map < cp$probmax] <- 1
          critzone_map_list[[cp$ll_id]] <- m
        } else {
          m <- !area_map
          m[randcritzone_map > cp$probmin & 
              randcritzone_map < cp$probmax &
              suit_map == 1] <- 1
          critzone_map_list[[cp$ll_id]] <- m
        }
      }
    
    }
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(25, "Calculate harvesting zone")
    #####################################################################
    
    #POTENTIAL HARVESTING ZONES AND AREAS FOR EACH LIVELIHOOD OPTION
    print("#POTENTIAL HARVESTING ZONES AND AREAS FOR EACH LIVELIHOOD OPTION")
    
    phzone_map_list <- list()
    dexistingplot_map_list <- list()
    harvestingarea_df <- params$livelihood_df[c("ll_id")]
    harvestingarea_df$harvestingarea <- 0
    map_ids <- as.data.frame(table(lc_map))
    for(ll_id in ll_df$ll_id) {
      lcids <- get_lcid_by_llid(ll_id)
      #if the map do not contain the lcids then skip 
      if(!(T %in% (lcids %in% map_ids[[1]]))) next
      if(is.null(lcids)) {
        phzone_map_list[[ll_id]] <- phzonecat_map_list[[ll_id]]
      } else {
        phzone_map_list[[ll_id]] <- !area_map
        phzone_map_list[[ll_id]][phzonecat_map_list[[ll_id]] & 
                                   lc_map %in% lcids] <- 1
      }
      ha <- sum(phzone_map_list[[ll_id]][[1]], na.rm = TRUE)
      harvestingarea_df$harvestingarea[harvestingarea_df$ll_id == ll_id] <- ha
        
      #report dexistingplot
      #kalo petanya kosong, jangan dihitung, error
      if(ha > 0)
        dexistingplot_map_list[[ll_id]] <- calcDistance(phzone_map_list[[ll_id]], 
                                                      target = 1)
    }
  
    #TO GENERATE SOIL FERTILITY PARAMETERS
    print("#TO GENERATE SOIL FERTILITY PARAMETERS")
    
    soildepletionrate_map <- reclassify_map(lc_map, parbio1_df[c("lc_id", "depletionrate")])
    soilrecoverytime_map <- reclassify_map(lc_map, parbio1_df[c("lc_id", "halftimerecovery")])
    
    #SOIL FERTILITY IS CODED BETWEEN 1-5. IF SOIL DEPLETION IS 1 THEN THE POTENTIAL YIELD
    #EQUALS REFERENCE YIELD. E.G. IF SOIL DEPLETION RATE IS 0.2 AND SOIL FERTILITY
    #EQUALS 5 THEN THE POTENTIAL EQUALS REFERENCE YIELD. THE MAXIMUM VALUE OF SOIL
    #DEPLETION IS THUS 1  
    
    soildepletion_map <- soildepletionrate_map * soilfert_map
    soildepletion_map[soildepletion_map > 1] <- 1
    soildepletion_map[soildepletion_map < 0] <- 0
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(30, "Calculate potential yield")
    #####################################################################
    
    #POTENTIAL YIELD
    print("#POTENTIAL YIELD")
    
    ylcids <- lc_df[lc_df$landuse == "Agriculture", "lc_id"]
    pyield_map_list <- list()
    yield_df <- ll_df["ll_id"]
    yield_df$potyield <- 0
    yield_df$nonlaborcosts <- 0
    
    #TODO: can be calculated once without loop?
    for(ll_id in ll_df$ll_id) {
      #####################################################################
      if(!is.null(progress_detail)) 
        progress_detail(30 + 20 * which(ll_df$ll_id == ll_id)/length(ll_df$ll_id), 
                        paste("Calculate potential yield:", ll_id))
      #####################################################################
      
      yltype_df <- yieldstat_df[!is.na(yieldstat_df$ll_id) & yieldstat_df$ll_id == ll_id,]
      #if there was no mean value, then nothing to compute
      if(sum(yltype_df$yield.mean, na.rm = T) == 0) next
      m <- rnorm_by_ids(lc_map, yltype_df$lc_id, yltype_df$yield.mean, yltype_df$yield.cv, 0)
      m_agr <- m * soildepletion_map
      m_agr[!(lc_map %in% ylcids) & !is.na(lc_map)] <- 0
      m[lc_map %in% ylcids] <- 0
      pyield_map_list[[ll_id]] <- m + m_agr
      
      lltype_df <- nonlaborcoststat_df[!is.na(nonlaborcoststat_df$ll_id) & nonlaborcoststat_df$ll_id == ll_id,]
      nlabcosts_rmap <- rnorm_by_ids(lc_map, lltype_df$lc_id,
                                                  lltype_df$nonlaborcost.mean, lltype_df$nonlaborcost.cv, 0)
      
      #THE ACTUAL (ATTAINABLE) YIELD IS LIMITED BY ALLOCATED LABOR 
      
      #report potyield
      if(!is.null(phzone_map_list[[ll_id]])) {
        potyield_map <- pyield_map_list[[ll_id]] * phzone_map_list[[ll_id]]
        yield_df$potyield[yield_df$ll_id == ll_id] <-
          sum(potyield_map[[1]], na.rm = TRUE)
      
        #CALCULATING ACTUAL REVENUE AND PROFIT  
  
        #report nonlaborcosts
        nlabcost_map <- nlabcosts_rmap * phzone_map_list[[ll_id]]
        nlabcost_sum <- sum(nlabcost_map[[1]], na.rm = TRUE)
        subsidy <- pareco1_df[pareco1_df$ll_id == ll_id, "subsidy_mntsub"]
 
        if(is.null(sub_df)) {
          yield_df$nonlaborcosts[yield_df$ll_id == ll_id] <- 
            ifelse(tssconsole, max(0, nlabcost_sum),
                   max(0, nlabcost_sum - subsidy))
        } else {
          sub <- sub_df$sub[sub_df$ll_id == ll_id]
          yield_df$nonlaborcosts[yield_df$ll_id == ll_id] <- 
            ifelse(tssconsole, max(0, nlabcost_sum - subsidy * sub),
                   max(0, nlabcost_sum - subsidy))
        }
      }
    }
  
    #report attyield
    yield_df <- merge(yield_df, availablelabor_df[c("ll_id", "availablelabor")], by = "ll_id")
    yield_df <- merge(yield_df, harvestingefficiency_df, by = "ll_id")
    yield_df$attyield <- pmin(yield_df$potyield, yield_df$availablelabor * yield_df$harvestingefficiency)
    
    totnonlaborcosts <- sum(yield_df$nonlaborcosts)
    # report revenue
    yield_df <- merge(yield_df, price_df, by = "ll_id")
    yield_df$revenue <- yield_df$attyield * yield_df$price
    # report profit
    yield_df <- merge(yield_df, extlabor_df, by = "ll_id")
    yield_df$extlaborcost <- yield_df$extlabor * 
      price_df$price[price_df$ll_id == OFFFARM] 
    yield_df$profit <- yield_df$revenue - yield_df$nonlaborcosts - yield_df$extlaborcost
  
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(50, "Calculate return to land and labor")
    #####################################################################
    
    #CALCULATING ACTUAL RETURN TO LAND AND TO LABOR
    print("#CALCULATING ACTUAL RETURN TO LAND AND TO LABOR")
    
    yield_df$payofftolabor <- ifelse(yield_df$availablelabor > 0, 
                                     yield_df$profit/yield_df$availablelabor, 0)
    yield_df <- merge(yield_df, harvestingarea_df, by = "ll_id")
    yield_df$payofftoland <- ifelse(yield_df$harvestingarea > 0 & yield_df$price > 0, 
                                    yield_df$profit/yield_df$harvestingarea, 0)
    totlaborcosts <- sum(yield_df$extlaborcost)
  
    #RETURN TO LAND AND TO LABOR FOR OFF-FARM
    
    yield_offfarm <- yield_df[yield_df$ll_id == OFFFARM,]
    yield_df$payofftolabor[yield_df$ll_id == OFFFARM] <-
      ifelse(yield_offfarm$availablelabor > 0, yield_offfarm$price, 0)
    yield_df$payofftoland[yield_df$ll_id == OFFFARM] <- 0
  
    #MARGINAL AGRICULTURE IS UNPRODUCTIVE CROP PLOTS 
    
    marginal_df <- exppayoff_df
    marginal_df$ismarglab <- marginal_df$exppayofftolabor <= 0
    marginal_df$ismarglan <- marginal_df$exppayofftoland <= 0
    marginal_df <- reshape(marginal_df[c("ll_id", "agent", "ismarglab", "ismarglan")], direction = "wide", idvar = "ll_id", 
                                          timevar = "agent")
    marginal_df <- merge(marginal_df, yield_df[c("ll_id", "profit")], by = "ll_id")
    marginal_df$ismarg <-  
      (marginal_df$ismarglan.agent1 & marginal_df$ismarglan.agent2) |
      (marginal_df$ismarglab.agent1 & marginal_df$ismarglab.agent2) |
      (marginal_df$profit <= 0)
    marginalagriculture_map <- area_map
    marginalagriculture_map[!is.na(marginalagriculture_map)] <- 0
    agr_lcids <- lc_df$lc_id[lc_df$landuse == "Agriculture"]
    agr_ltypes <- ll_df$ll_id[ll_df$lu_group == "Agriculture" & !is.na(ll_df$lu_group)]
    agr_df <-  data.frame(agr_lcids, agr_ltypes)
    for(lc_id in agr_lcids) {
      ll_id <- agr_df$agr_ltypes[agr_df$agr_lcids == lc_id]
      if(marginal_df$ismarg[marginal_df$ll_id == ll_id]) {
        marginalagriculture_map[lc_map == lc_id] <- 1
      } else {
        marginalagriculture_map[lc_map == lc_id & pyield_map_list[[ll_id]] <= 0.5*pixelsize ] <- 1
      }
    }
    
    #MARGINAL AF IS UNPRODUCTIVE AGRO-FORESTRY  OR TIMBER BASED SYSTEM
    #IF THE SYSTEM HAS REACH MATURE OR POST PRODUCTION STAGE AND REMAIN
    #UNPROFITABLE THEN WILL BE AUTOMATICALLY CONVERTED INTO PIONEER FOREST  
    
    marginalAF_map <- area_map
    marginalAF_map[!is.na(marginalAF_map)] <- 0
    lc_tree_df <- lc_df[lc_df$landuse == "Tree-based system" & lc_df$growth_stage %in% c("Mature", "Old"),]
    llids <- unique(lc_tree_df$ll_id)
    
    for (ll_id in llids) {
      profit <- yield_df$profit[yield_df$ll_id == ll_id]
      lcids <- lc_tree_df$lc_id[lc_tree_df$ll_id == ll_id]
      marginalAF_map[lc_map %in% lcids] <- profit < 0 
    }
    
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(55, "Calculate biomass")
    #####################################################################
    
    #CALCULATING ABOVEGROUND BIOMASS, FLOOR BIOMASS, AND FIRE EVENT
    print("#CALCULATING ABOVEGROUND BIOMASS, FLOOR BIOMASS, AND FIRE EVENT")

    agbiomass_map <- rnorm_by_ids(lc_map, parbio1_df$lc_id, parbio1_df$agb.mean,
                                  parbio1_df$agb.cv, default_val = 0)
    
    floorbiomassfraction_map <- rnorm_by_ids(lc_map, parbio1_df$lc_id, 
                                             parbio1_df$floorbiomassfrac.mean,
                                             parbio1_df$floorbiomassfrac.cv, 
                                             default_val = 0)
    
    pfireescape_map <- rnorm_by_ids(lc_map, parbio1_df$lc_id, 
                                    parbio1_df$pfirespread.mean,
                                    parbio1_df$pfirespread.cv, default_val = 0)
    
    #YIELD OF LOGGING 
    
    ha_timber <- yield_df$harvestingarea[yield_df$ll_id == TIMBER]
    ay_timber <- yield_df$attyield[yield_df$ll_id == TIMBER]
    
    loggedtimber_map <- area_map
    loggedtimber_map[!is.na(loggedtimber_map)] <- 0
    if(ha_timber > 0) {
      loggedtimber_map <- phzone_map_list$timber * ay_timber/ha_timber  
    }  
    
    #CALCULATING TOTAL C STOCK IN THE LANDSCAPE
    print("#CALCULATING TOTAL C STOCK IN THE LANDSCAPE")
    
    b_to_c <- with(params, converter_df$value[converter_df$field == "biomass_to_c"])
    if(is.na(b_to_c)) {
      b_to_c <- 0.5
    } 
    agcarbon_map <- agbiomass_map * b_to_c
    
    floorbiom_map <- agbiomass_map * floorbiomassfraction_map
    totagb <- sum(agbiomass_map[[1]], na.rm = TRUE)
    #report totagc 
    totagc <- sum(agcarbon_map[[1]], na.rm = TRUE)
    
    #UPDATING THE FOOD STORAGE
    store_df <- store_df[ , !(names(store_df) %in% c("attyield"))]
    store_df <-  merge(store_df, yield_df[c("ll_id", "attyield")], 
                       by = "ll_id")
    store_df$store <- pmax(0, store_df$store * (1 - store_df$lossfrac) + 
                               store_df$attyield)
    
    #CALCULATION OF PLOT ATTRACTIVENESS. ALL DETERMINANT SHOULD BE STANDARDIZED (0-1)
    #BEFORE CALCULATING THE ATTRACTIVENESS INDEX. THE DETERMINANTS ARE SOIL FERTILITY,
    #DISTANCE TO EXISTING PLOT, PLOT YIELD, FLOOR BIOMASS
    
    #standarization of soil fertility
    zfert_map <- standardize_map(soilfert_map)
    
    #standarization of distance to existing plot 
    zdplot_map_list <- list()
    for(ll_id in ll_df$ll_id) {
      if(ll_id <= length(dexistingplot_map_list))
        zdplot_map_list[[ll_id]] <- standardize_map(dexistingplot_map_list[[ll_id]])
    }
    
    #standarization of plot yield  
    yield_df$zyield <- yield_df$attyield/max(yield_df$attyield)
    
    #standarization of floor biomass
    zfb_map <- standardize_map(floorbiom_map)
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(60, "Calculate plot attactiveness")
    #####################################################################
    
    #CALCULATION OF PLOT ATTRACTIVENESS
    print("#CALCULATION OF PLOT ATTRACTIVENESS")
    
    min_d_map <- area_map
    road_m <- getProximityMap("Road proximity")
    river_m <- getProximityMap("River proximity")
    mart_m <- getProximityMap("Market proximity")
    min_d_map[[1]] <- pmin(road_m[[1]], river_m[[1]], mart_m[[1]], na.rm = TRUE)
    settlement_map <- getProximityMap("Settlement proximity")
    
    attr_map_list <- list()
    zattrclass_map_list <- list()
    zfreq_df <- data.frame(matrix(nrow = 0, ncol = 3))
   
    af_ltypes <- ll_df$ll_id[ll_df$lu_group == "Tree-based system"]
    for(ll_id in ll_df$ll_id) {  
      #####################################################################
      if(!is.null(progress_detail)) 
        progress_detail(60 + 20 * which(ll_df$ll_id == ll_id)/length(ll_df$ll_id), 
                        paste("Calculate plot attactiveness:", ll_id))
      #####################################################################
      #TODO: loop berdasarkan critzone_map aja!
      if(ll_id > length(critzone_map_list)) next
      critzone_map <- critzone_map_list[[ll_id]]
      if(is.null(critzone_map)) next
      if(ll_id == OFFFARM) next
      suit_maps <- get_suitability_map(ll_id)
      if(is.null(suit_maps)) next
      
      fert_f <- getSpatialW("fertility", ll_id) * zfert_map
      yield_f <- getSpatialW("yield", ll_id) * 
        yield_df$zyield[yield_df$ll_id == ll_id]
      suit_f <- getSpatialW("suitability", ll_id) * suit_maps
      min_d_ll <- !area_map
      min_d_ll[[1]] <- pmin(min_d_map[[1]], getMapll(ll_id)[[1]], na.rm = TRUE) 
      
      transp_f <- getSpatialW("transportation", ll_id) * min_d_ll 
      min_m <- !area_map
      if(!is.null(zdplot_map_list[[ll_id]]))
        # min_m[[1]] <- pmin(map_data$zset[[1]], zdplot_map_list[[ll_id]][[1]], na.rm = TRUE)
        min_m[[1]] <- pmin(settlement_map[[1]], zdplot_map_list[[ll_id]][[1]], na.rm = TRUE)
      maint_f <- getSpatialW("maintenance", ll_id) * min_m 
      steep_f <- getSpatialW("steepness", ll_id) * zslope_map
      floorb_f <- getSpatialW("floorbiomass", ll_id) * zfb_map
      
      attr_f <- (fert_f + yield_f + suit_f)/
        (1 + transp_f + maint_f + steep_f + floorb_f)
      
      attr_map_list[[ll_id]] <- attr_f
      attr_map_list[[ll_id]][reserve_map] <- 0 
      attr_map_list[[ll_id]][!critzone_map] <- NA #attractiveness only apply to critical zone
      if(ll_id %in% agr_ltypes) {
        attr_map_list[[ll_id]] <- attr_map_list[[ll_id]] * !marginalagriculture_map
      } else if(ll_id %in% af_ltypes){
        attr_map_list[[ll_id]] <- attr_map_list[[ll_id]] * !marginalAF_map
      }
  
      #STANDARDIZING THE ATTRACTIVENESS INDEX ACCORDING TO NORMAL DISTRIBUTION
      #STANDARDIZATION PROCEDURE
      
      meanattr <- mean(attr_map_list[[ll_id]][[1]], na.rm = TRUE)
      sdattr <- sd(attr_map_list[[ll_id]][[1]], na.rm = TRUE)
      zattr_map <- (attr_map_list[[ll_id]] - meanattr)/sdattr
      
      #THE STANDARDIZED ATTRACTIVENESS INDEXES ARE THEN CLASSIFIED INTO 5 CLASSES
      #INDEXES <0,0-1,1-2,2-3,>3. THE HIGHEST VALUE IS USUALLY 4 BECAUSE THE LARGEST VALUE 
      #IN THE STANDARD NORMAL DISTRIBUTION IS AROUND 4. THE HIGHER THE VALUE THE MORE ATTRACTIVE 
      #THE PLOTS 
      
      class_map <- zattr_map
      class_map[zattr_map < 0] <- 1
      class_map[zattr_map >= 0 & zattr_map < 1] <- 2
      class_map[zattr_map >= 1 & zattr_map < 2] <- 3
      class_map[zattr_map >= 2 & zattr_map < 3] <- 4
      class_map[zattr_map >= 3] <- 5
      zattrclass_map_list[[ll_id]] <- class_map
      
      #CALCULATING THE NUMBER OF PLOT FOR EACH ATTRACTIVENESS CLASS
      zf <- data.frame(table(class_map))
      if(nrow(zf) == 0) next
      zf$ll_id <- ll_id
      zfreq_df <- rbind(zfreq_df, zf)
      
    }
    colnames(zfreq_df) <- c("zclass", "zfreq", "ll_id")
    
    getZfreq <- function(ll_id, zclass) {
      f <- zfreq_df$zfreq[zfreq_df$ll_id == ll_id & 
                              zfreq_df$zclass == zclass]
      return(ifelse(length(f) == 0, 0, f))
    }
    
    #CALCULATING NUMBER OF PLOT BEYOND A CERTAIN CLASS
    zexc_df <- data.frame(matrix(nrow = 0, ncol = 3))
    for(ll_id in ll_df$ll_id) {
      if(!(ll_id %in% zfreq_df$ll_id)) next
      zexc <- vector()
      zexc[5] <- 0
      zexc[4] <- zexc[5] + getZfreq(ll_id, 5)
      zexc[3] <- zexc[4] + getZfreq(ll_id, 4)
      zexc[2] <- zexc[3] + getZfreq(ll_id, 3)
      zexc[1] <- zexc[2] + getZfreq(ll_id, 2)
      zdf <- data.frame(ll_id, c(1:5), zexc)
      zexc_df <- rbind(zexc_df, zdf)
    }
    colnames(zexc_df) <- c("ll_id", "zclass", "zexc")
    
    #CALCULATING AVAILABLE MONEY FOR EACH LIVELIHOOD OPTION BASED ON THE LABOR
    #MONEY FRACTION CALCULATED BEFORE (i.e. BASED ON RETURN TO LABOR, CULTURAL
    #DELIBERATION AND PRIORITIZATION DEGREE. BECAUSE THE AVAILABLE MONEY HERE 
    #WILL BE USED FOR LIMITING PLOT EXPANSION THEN IT SHOULD BE ADDED BY SUBSIDY
    #FOR PLOT ESTABLISHMENT. BALANCE SHOULD BE CORRECTED BY TOTAL NON-LABOR COST
    #BECAUSE FIRST OF ALL AVAILABLE MONEY HAS TO BE USED TO MAINTAIN EXISTING PLOTS 
    print("#CALCULATING AVAILABLE MONEY FOR EACH LIVELIHOOD OPTION")
    
    labormoneyfrac_df$availablemoney <- labormoneyfrac_df$labormoneyfrac * balance
    availablemoney_df <- aggregate(labormoneyfrac_df$availablemoney,
                                   by=list(Category=landfrac_df$ll_id),
                                                        FUN=sum)
    names(availablemoney_df) <- c("ll_id", "availablemoney")
    
    if(!is.null(sub_df))
      availablemoney_df <- merge(availablemoney_df, sub_df, by = "ll_id")
    estsub_df <- pareco1_df[c("ll_id", "subsidy_estsub")]
    names(estsub_df) <- c("ll_id", "estsub")
    availablemoney_df <- merge(availablemoney_df, estsub_df, by = "ll_id")
    #report availablemoney
    if(tssconsole) {
      availablemoney_df$availablemoney <- with(availablemoney_df, 
                                               availablemoney + estsub * sub)
    } else {
      availablemoney_df$availablemoney <- with(availablemoney_df, 
                                               availablemoney + estsub)
    }
    
    #RELATIVE AVAILABLE MONEY AND LABOR FOR PLOT EXPANSION. THE ACTUAL AREA OF 
    #PLOT EXPANSION DEPENDS ON AVAILABLE LANDS, MONEY AND LABOR
    
    exparea_df <- merge(availablelabor_df[c("ll_id", "availablelabor")],
                         estlabor_df, by = "ll_id")
    exparea_df <- merge(exparea_df, 
                        availablemoney_df[c("ll_id", "availablemoney")], 
                        by = "ll_id")
    exparea_df <- merge(exparea_df, estcost_df, by = "ll_id")
    exparea_df <- merge(exparea_df, critzonearea_df, by = "ll_id")
    exparea_df$exparealabor <- with(exparea_df, 
                                    ifelse(estlabor > 0, availablelabor/estlabor, 0))
    exparea_df$expareamoney <- with(exparea_df, 
                                    ifelse(estcost > 0, availablemoney/estcost, 0))
    exparea_df$exparea <-with(exparea_df, pmin(exparealabor, expareamoney, critzonearea))
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(80, "Calculate expansion probability")
    #####################################################################
    
    #CALCULATION OF EXPANSION PROBABILITY. THE PLOT BECOMES A NEW PLOT IF 
    #THE RANDOM NUMBER IN THE ATTRACTIVE CRITICAL ZONES IS LOWER THAN
    #THE EXPANSION PROBABILITY VALUES 
    print("#CALCULATION OF EXPANSION PROBABILITY")
    
    expprob_df <- zexc_df
    expprob_df <- merge(expprob_df, zfreq_df, by = c("ll_id", "zclass"), all.y = TRUE)
    expprob_df <- merge(expprob_df, exparea_df[c("ll_id", "exparea")], 
                        by = c("ll_id"))
    expprob_df$expprob <- with(expprob_df, 
                               ifelse(zfreq > 0, 
                                      pmax(0, pmin(zfreq, exparea - zexc))/zfreq, 0))
    
    allnewplots_map <- !area_map
    newplot_map_list <- list()
    allfireignition_map <- !area_map
    newplotarea_df <- data.frame(ll_id = unique(expprob_df$ll_id))
    totestcost <- 0

    if(nrow(newplotarea_df) > 0) {
      newplotarea_df$newplotarea <- 0
      for(ll_id in newplotarea_df$ll_id) {
        
        #####################################################################
            if(!is.null(progress_detail)) {
              llt <- newplotarea_df$ll_id
              progress_detail(80 + 10 * which(llt == ll_id)/length(llt), 
                          paste("Calculate expansion probability:", ll_id))
            }
        #####################################################################
        
        expansionprobability_map <- !area_map
        zattrclass_map <- zattrclass_map_list[[ll_id]]
        expprob_l <- expprob_df[expprob_df$ll_id == ll_id, c("zclass", "expprob")]
        for(zclass in expprob_l$zclass) {
          expansionprobability_map[zattrclass_map == zclass] <- 
            expprob_l$expprob[expprob_l$zclass == zclass]
        }
        expansionprobability_map <- expansionprobability_map * critzone_map_list[[ll_id]]
        
        rand_map <- runif_to_map(area_map)
        rand_map[area_map != 1] <- NA
        
        newplot_map_list[[ll_id]] <- !area_map
        newplot_map_list[[ll_id]][rand_map < expansionprobability_map & !reserve_map] <- 1
        #report newplotarea
        newplotarea_df$newplotarea[newplotarea_df$ll_id == ll_id] <- 
          sum(newplot_map_list[[ll_id]][[1]], na.rm = TRUE) * pixelsize
        
        #SLASH AND BURN EVENT IN THE NEW PLOT 
        
        rand_map <- runif_to_map(newplot_map_list[[ll_id]])
        rand_map[newplot_map_list[[ll_id]] != 1] <- NA
        pfireuse <- pfireuse_df$pfireuse[pfireuse_df$ll_id == ll_id]
        allfireignition_map[rand_map < pfireuse] <- 1
        
        #MAP DESCRIVING AREAS OF NEW PLOTS
        
        allnewplots_map[newplot_map_list[[ll_id]] == 1] <- 1
      }
      
      #TOTAL ESTABLISHMENT COST FOR ALL NEW PLOTS 
      newplotarea_df <- merge(newplotarea_df, estcost_df, by = "ll_id")
      newplotarea_df$newestcost <- with(newplotarea_df, estcost * newplotarea/pixelsize) 
      
      #report totestcost
      totestcost <- sum(newplotarea_df$newestcost)
    
    }
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(90, "Calculate market")
    #####################################################################
    
    #CALCULATION OF BUYING, SELLING AND SECURITY LEVEL
    #PART OF THE STORE IS FOR SATISFYING DEMAND, THE REST CAN BE SOLD 
    #TO THE MARKET OR KEEP STORED. USUALLY DEMAND IS FOR STAPLE FOOD ONLY. 
    print("#CALCULATION OF BUYING, SELLING AND SECURITY LEVEL")
    
    market_df <- store_df
    market_df <- merge(market_df, price_df, by = "ll_id")
    market_df$totdemand  <- totpop * market_df$demandpercapita
    market_df$balprice  <- ifelse(market_df$price > 0, balance/market_df$price, 0)
    #report buying
    market_df$buying <- with(market_df, pmin(
      pmax(0, totdemand - store * (1 - lossfrac)), balprice))
    #report selling
    market_df$selling <- with(market_df, 
                              pmax(0, store * (1 - lossfrac) - totdemand)*ptosell) 
    market_df$supplysufficiency <- 
      with(market_df, pmax(0, pmin(1, ifelse(totdemand <= 0, 0, 
                             1 - (totdemand - store + buying - selling)/totdemand))))

    # update storage for the next iteration
    store_df$store <- 
      with(market_df, pmax(0, store * (1 - lossfrac) + buying - totdemand - selling))

    #UPDATING MONEY CAPITAL
    
    totbuying <- sum(market_df$buying * market_df$price)
    totselling <- sum(market_df$selling * market_df$price)
    totnetincome <- max(0, totselling-totbuying-totnonlaborcosts-totlaborcosts-totestcost)
    totnetincome_df <- rbind(totnetincome_df, list(totnetincome = totnetincome, 
                                                   totselling = totselling, 
                                                   totbuying = totbuying, 
                                                   totnonlaborcosts = totnonlaborcosts, 
                                                   totlaborcosts = totlaborcosts, 
                                                   totestcost = totestcost))
    
    totsecconsumption <- max(0, totnetincome * getVarValue(demographics_df, "secconsumptionfrac"))
                               # demographics_df$value[demographics_df$field == "secconsumptionfrac"])
    #report totsecconsumptionpercapita
    totsecconsumptionpercapita <- ifelse(totpop > 0, totsecconsumption/totpop, 0)      
    #report totnetincomepercapita
    totnetincomepercapita <- ifelse(totpop > 0, totnetincome/totpop, 0)   
    balance <- balance + totnetincome - totsecconsumption
    balance <- balance * (1- (disasterimpactonmoney/100) )   
    
    #MARGINAL AGRICULTURE AND MARGINAL AF PLOT THAT ARE NOT SELECTED AS NEW PLOT 
    #WILL BE LATER AUTOMATICALLY CONVERTED INTO PIONEER FOREST TYPE
    
    nonselectedagricplot_map <- !area_map
    nonselectedagricplot_map[!allnewplots_map & 
                               (marginalagriculture_map | marginalAF_map)] <- 1
    
    #FIRE EVENT THAT CAN SPREAD TO NEIGHBORING PLOTS
  
    firearea <- 0
    fire_map <- !area_map
    dfireignition_map <- calcDistance(allfireignition_map, target = 1)
    if(!is.null(dfireignition_map)) {
      rand_map <- runif_to_map(area_map)
      rand_map[area_map != 1] <- NA
      ig <- 2 * pixelsize ^ 0.5
      rand_map[!(dfireignition_map <= ig)] <- 0
      fire_map[rand_map < pfireescape_map | allfireignition_map == 1] <- 1
      firearea <- sum(fire_map[[1]], na.rm = TRUE)
    }
    
    #TODO: ?
    #ntfpzone=newplot[ntfp]; #newplot = actual area yg di konversi berdasarkan available money, labour
    
    #UPDATING HUMAN POPULATION
    
    #report totpop
    totpop <- totpop * (1 + getVarValue(demographics_df, "annualgrowthrate")) *
      (1 - disasterimpactonhuman/100)
    
    #UPDATING EXPECTED RETURN TO LABOR AND TO LAND BASED ON NEW OBTAINED INFORMATION
    #FARMER EXPECTATION CAN BE INFLUENCED BY THE ACTUAL PROFIT AND SUGGESTION FROM OTHERS
    print("#UPDATING EXPECTED RETURN TO LABOR AND TO LAND BASED ON NEW OBTAINED INFORMATION")
    
    exppayoff_df <- merge(
      exppayoff_df[c("ll_id", "agent","exppayofftolabor","exppayofftoland")], 
      yield_df[c("ll_id", "payofftolabor", "payofftoland")], 
      by = "ll_id")
    exppayoff_df <- merge(exppayoff_df, agentprop_df, by = "agent")
    exppayoff_df <- merge(exppayoff_df, extensionprop_df, by = "ll_id")
    exppayoff_df <- merge(exppayoff_df, extensionsuggestion_df, by = "ll_id")
    if(!is.null(ex_df))
      exppayoff_df <- merge(exppayoff_df, ex_df, by = "ll_id")
    
    exppayoff_df$exavail <- ifelse(tssconsole, exppayoff_df$ex, exppayoff_df$event)
    exppayoff_df$paylaboralpha <- 
      with(exppayoff_df, 
           exppayofftolabor + alpha_learning * (payofftolabor - exppayofftolabor))
    exppayoff_df$payofftolabor <- 
      with(exppayoff_df, ifelse(payofftolabor <= 0, 0,  
           paylaboralpha + 
             beta_learning * exavail * credibility * exposure * 
             (explabor - paylaboralpha)))
    exppayoff_df$paylandalpha <- 
      with(exppayoff_df, 
           exppayofftoland + alpha_learning * (payofftoland - exppayofftoland))
    exppayoff_df$payofftoland <- 
      with(exppayoff_df, ifelse(payofftoland <= 0, 0,
           paylandalpha + 
             beta_learning * exavail * credibility * exposure * 
             (expland - paylandalpha)))
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(92, "Calculate soil fertility")
    #####################################################################
    
    #UPDATING SOIL FERTILITY
    maxsoilfert_map <- get_map("maxsoilfert_map")
    soilrecovery_div_map <- (1 + soilrecoverytime_map) * maxsoilfert_map - soilfert_map
    soilrecovery_map <- (maxsoilfert_map - soilfert_map)^2/soilrecovery_div_map
    soilrecovery_map[soilrecovery_div_map < 0] <- 0

    soilfert_map <- soilfert_map + soilrecovery_map - soildepletion_map
    soilfert_map[soilfert_map < 0] <- 0
    maxsoilfert_map[is.na(soilfert_map)] <- NA
    soilfert_map[!is.na(soilfert_map) & soilfert_map > maxsoilfert_map] <- maxsoilfert_map
      
    #UPDATING LANDUSE TYPE. THE NON-CONVERTED MARGINAL AGRICULTURE AND MARGINAL AF
    #WILL BE AUTOMATICALLY CONVERTED INTO PIONEER FOREST TYPE
    print("#UPDATING LANDUSE TYPE")
    
    #TODO: the forest land use can have more than one id
    forest_lu_id <- min(lu_df$lu_id[lu_df$lu_group == "Forest"])
    
    lu_update_map <- lu_map
    lu_update_map[fire_map == 1 & !(allnewplots_map == 1)] <- forest_lu_id
    lu_update_map[disasterimpactedzone_map == 1] <- forest_lu_id
    lu_update_map[nonselectedagricplot_map == 1] <- forest_lu_id
    
    if(length(newplot_map_list) > 0) { 
      for(i in 1:length(newplot_map_list)) {
        if(is.null(newplot_map_list[[i]])) next
        lu_update_map[newplot_map_list[[i]] == 1] <- ll_df$lu_id[ll_df$ll_id == i]
      }
    }
    lu_map <- lu_update_map
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(95, "Calculate forest age")
    #####################################################################
    
    #UPDATING THE AGE OF FOREST LANDCOVER
    print("#UPDATING THE AGE OF FOREST LANDCOVER")
    
    forest_df <- lc_df[lc_df$landuse == "Forest",] 
    forest_df <- merge(forest_df, params$bio_lc_df[
      c("lc_id", "agb.mean", "initlcage.mean", "initlcage.cv")], 
      by = "lc_id", all.x = T)
    forest_df$agb_max <- c(forest_df$agb.mean[-1], .Machine$double.xmax)
    forest_df$agb_min <- c(0, forest_df$agb.mean[1:(nrow(forest_df)-1)])
    
    agebasedbiomass_map <- !area_map
    rnorm_map <- rnorm_to_map(area_map)
    for(i in 1:nrow(forest_df)) {
      a_max <- forest_df$agb_max[i]
      a_min <- forest_df$agb_min[i]
      m <- forest_df$initlcage.mean[i]
      c <- forest_df$initlcage.cv[i]
      initlcage_map <- m + rnorm_map * c * m
      initlcage_map[!(agbiomass_map >= a_min & agbiomass_map < a_max)] <- 0
      agebasedbiomass_map <- agebasedbiomass_map + initlcage_map
    }
    agebasedbiomass_map[agebasedbiomass_map < 0] <- 0
    
    #UPDATING LANDCOVER AGE
    lcage_map <- lcage_map + 1
    lcage_map[allnewplots_map == 1] <- 0
    lcage_map[fire_map == 1] <- 0
    lcage_map[disasterimpactedzone_map == 1] <- 0
    lcage_map[nonselectedagricplot_map == 1] <- 0
    lcage_map[!is.na(lcage_map) & phzone_map_list[[TIMBER]] == 1] <- agebasedbiomass_map

    totfinance <- balance
    
    #####################################################################
    if(!is.null(progress_detail)) progress_detail(100, "Saving the output")
    #####################################################################
    

    
    mlist <- list(
      lu_map = lu_map,
      lc_map = lc_map,
      soilfert_map = soilfert_map,
      agbiomass_map = agbiomass_map,
      agcarbon_map = agcarbon_map,
      fire_map = fire_map,
      lcage_map = lcage_map
    )
    
    for(m_id in out_map_df$id) {
      m_new <- mlist[[m_id]]
      names(m_new) <- paste0(out_map_df$label[out_map_df$id == m_id], " [", time, "]")
      m <- out_map[[m_id]]
      if(is.null(m)) {
        out_map[[m_id]] <- m_new
      } else {
        out_map[[m_id]] <- c(m, m_new)
      }
    }
      
    ####### OUTPUT #############


    olc <- lcarea_df
    olc$iteration <- time
    if(is.null(out_lc_df)) {
      # print(olc)
      out_lc_df <- olc[c("lc_id",	"lu_id", "iteration", "area")]
    } else {
      out_lc_df <- bind_rows(out_lc_df, olc)
    }  
    
    out_list <- list(
      market_df = list(market_df), 
      availablelabor_df = list(availablelabor_df), 
      availablemoney_df = list(availablemoney_df), 
      yield_df = list(yield_df), 
      critzonearea_df = list(critzonearea_df), 
      exparea_df = list(exparea_df), 
      newplotarea_df = list(newplotarea_df) 
    )

    oll <- ll_df["ll_id"]
    oll$iteration <- time
    
    merge_out <- function(t) {
      v <- out_df$var[out_df$table == t]
      df <- out_list[[t]][[1]]
      if(nrow(df) == 0) return()
      oll <<- merge(oll, df[c("ll_id", v)], by = "ll_id", all.x = T)
    }
    lapply(names(out_list), merge_out)
    
    w_exppayoff_df <- exppayoff_df[c("ll_id", "agent", "payofftolabor", "payofftoland")]
    w_exppayoff_df <- reshape(w_exppayoff_df, timevar = "agent", idvar = "ll_id", direction = "wide")
    oll <- merge(oll, w_exppayoff_df, by = "ll_id", all.x = T)
    
    if(is.null(out_ll_df)) {
      out_ll_df <- oll
    } else {
      out_ll_df <- bind_rows(out_ll_df, oll)
    } 
   
    if(is.null(out_val_df)) {
      out_val_df <- data.frame(
        iteration = time,
        totsecconsumptionpercapita = totsecconsumptionpercapita,
        totnetincomepercapita = totnetincomepercapita,
        totestcost = totestcost,
        totpop = totpop,
        totagb = totagb,
        totagc = totagc,
        firearea = firearea
        
        # totbuying = totbuying,
        # totselling = totselling,
        # totnetincome = totnetincome,
        # totsecconsumption = totsecconsumption
      )
      
    } else {
      v <- c(time, totsecconsumptionpercapita, totnetincomepercapita, 
             totestcost, totpop, totagb, totagc, firearea)
             
             # totbuying, totselling, totnetincome, totsecconsumption)

      out_val_df <- rbind(out_val_df, v)
    } 

    if(!is.null(progress_detail)) progress_detail(100, "Finish iteration")
  }
  
  time_elapsed <- round(Sys.time() - start_time, 2)
  
  if(!is.null(progress_iteration)) {
    progress_iteration(
      time, paste0("Iteration [elapsed time: ", format(time_elapsed, nsmall = 2),"]"))
  }
  
  output <- list()
  output[["out_map"]] <- out_map
  output[["out_lc_df"]] <- out_lc_df
  output[["out_ll_df"]] <- out_ll_df
  output[["out_val_df"]] <- out_val_df
  output[["time_elapsed"]] <- time_elapsed
  print("*****************")
  print("ITERATION FINISH!")
  print("*****************")
  return(output)
}

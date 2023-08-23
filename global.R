### RFALLOW global variables ########################
library(dplyr)


COLOR_DARK <- "#385624"
COLOR_LIGHT <- "#C6E0B3"

landuse_list <- c("Forest", "Tree-based system", "Agriculture", "Settlement")
landuse_conv <- c("for", "af", "agr", "set")
landuse_short <- c("FOR", "TRE", "AGR", "SET")
growth_stage_list <- c("Pioneer", "Young", "Mature", "Old")
growth_stage_conv <- list(Pioneer = c("pion"), 
                          Young = c("ysec", "eprod"), 
                          Mature = c("osec", "lprod"), 
                          Old = c("prim", "pprod"))

lc_field <- c("lc_id", "color", "landuse", "landcover", "growth_stage",
                      "lc_short", "lu_id", "ll_id")
lc_field_display <- c("lc_id", "color", "landuse", "landcover", "growth_stage",
                      "lc_short")
lc_column <- data.frame(
  title=c("LC_ID", "Color", "Land use", "Land cover", "Growth stage", "LC Short"),
  type=c("numeric", "color", "dropdown", "text", "dropdown", "text"),
  render=c(NA, "square", NA, NA, NA, NA),
  align = c("right", "center", "left", "left", "left", "left"),
  source=I(list(0, 0, landuse_list, 0, growth_stage_list, 0)),
  width = c(NA, NA, 150, 150, 150, 120),
  readOnly = c(F, F, F, F, F, T)
)

lu_field <- c("lu_id", "color", "landuse", "lu_short", "lu_group")
lu_field_display <- c("lu_id", "color", "landuse", "lu_short")
lu_column <- data.frame(
  title=c("LU_ID", "Color", "Land use", "LU Short"),
  type=c("text", "color", "text", "text"),
  render=c(NA, "square", NA, NA),
  align = c("right", "center", "left", "left"),
  width = c(NA, NA, 200, 120),
  readOnly = c(T, F, T, T)
)

match_growth_stage <- function(x){
  if(x == "") return(x)
  s <- lapply(growth_stage_conv, function(y){x %in% y})
  names(s[s == T])
}

livelihoodtype_base <- c("Off-farm", "NTFP", "Timber")
livelihoodtype_base_group <- c("", "Forest", "Forest")
OFFFARM <- 1
NTFP <- 2
TIMBER <- 3


ll_field <- c("ll_id", "livelihood", "ll_short", "lu_id", "lu_group")
ll_field_display <- c("ll_id", "livelihood", "ll_short")
ll_column <- data.frame(
  title=c("LL_ID", "livelihood", "LL short"),
  type=c("text", "text", "text"),
  align = c("right", "left", "left"),
  width = c(NA, NA, 100),
  readOnly = c(T, T, T)
)

## MAPS INPUT ############
map_dist_label_base <- c("Road proximity", "River proximity",
                         "Market proximity", "Settlement proximity")

map_input_df <- data.frame(
  box_id = c("general", "suitability", "proximity"),
  box_title = c("General Data", "Land Suitability", "Proximity Map"),
  box_desc = c(
    "The general spatial raster data input",
    "The land suitability map for each land uses",
    "The proximity (raster distance) to road, river, market, settlement and processing industries")
)

map_input_footer <- "* double click on the dragged map item to display the map"

general_map_ids <- c("initsoilfert_map", "maxsoilfert_map", "slope_map", 
                     "subcatchment_map", "initlog_map", "reserve_map", "disaster_map")

general_map_old_ids <- c("initsoilfert", "maxsoilfert", "slope", 
                     "subcatchment", "initlog", "reserve", "disaster")


map_data_df <- data.frame(
  label = c("Initial soil fertility", "Max soil fertility", "Slope map", 
            "Subcatchment area", "Logging zone", "Reserved area", "Disaster area"),
  id = factor(general_map_ids, levels = general_map_ids),
  group = rep("general", 7),
  file = NA,
  ll_id = ""
)

## SCALAR INPUT ##########

tab_scalar_df <- data.frame(
  # tab = c("inp_biophysic_lc", "inp_biophysic_ll"),
  title = c("Biophysic Parameters by Land Cover Classes",
            "Biophysic Parameters by Livelihood Types",
            "Economic Parameters by Land Cover Classes",
            "Economic Parameters by Livelihood Types",
            "Socio-Cultural Parameters"
            ),
  table = c("bio_lc_df", "bio_ll_df", "eco_lc_df", "eco_ll_df", "soc_ll_df")
)

par_scalar_title_df <- data.frame(
  id = c("lctimebound", "initlcage", "depletion", "agb", "floorbiomass", "fire",
         "harvest", "store", "expansion", "nonlaborcost", "yield", "price", "initknowledge", "subsidy",
         "estcost", "estlabor", "extlabor", "culturaldeliberation",
         "extensionprop", "extensionsuggestion", "pfireuse"),
  title = c("Land cover time bound", "Initial of land cover age",
            "Soil depletion rate", "Aboveground biomass", "Floor biomass",
            "Fire spreading potential",

            "Harvesting", "Store properties","Expansion determinant",

            "Non labor cost", "Yield",

            "Price", "Initial knowledge", "Subsidy", "Plot establishment cost",
            "Plot establishment labor", "External labor",

            "Cultural deliberation", "Extension property",
            "Extension suggestion", "Potential of fire use"
            ),
  width = c(rep(6, 6), 5, 7, 12, 6, 6, 5, 7, rep(6, 4), 5,7,7,5),
  table = c(rep("bio_lc_df", 6), rep("bio_ll_df", 3), rep("eco_lc_df", 2), rep("eco_ll_df", 6), rep("soc_ll_df", 4)),
  table_id = c(rep("lc_id", 6), rep("ll_id", 3), rep("lc_id", 2), rep("ll_id", 6), rep("ll_id", 4)),
  table_id_str = c(rep("lc_short", 6), rep("ll_short", 3), rep("lc_short", 2), rep("ll_short", 6), rep("ll_short", 4)),
  table_id_str_label = c(rep("Land cover", 6), rep("Livelihood", 3), rep("Land cover", 2), rep("Livelihood", 6), rep("Livelihood", 4))
)

par_scalar_field_df <- data.frame(
  id = c("lctimebound", "lctimebound", "initlcage", "initlcage",
         "depletion", "depletion", "agb", "agb", "floorbiomass", "floorbiomass",
         "fire", "fire",

         "harvest", "harvest", rep("store", 3), rep("expansion", 7),

         "nonlaborcost", "nonlaborcost", "yield", "yield",

         "price", "price", rep("initknowledge", 4), "subsidy", "subsidy",
         "estcost", "estcost", "estlabor", "estlabor", "extlabor", "extlabor",

         "culturaldeliberation", rep("extensionprop", 3),
         "extensionsuggestion", "extensionsuggestion","pfireuse"
         ),
  field = c("lctimebound.min", "lctimebound.max", "initlcage.mean", "initlcage.cv",
            "depletionrate", "halftimerecovery", "agb.mean", "agb.cv",
            "floorbiomassfrac.mean", "floorbiomassfrac.cv", "pfirespread.mean", "pfirespread.cv",

            "harvestingstat.mean", "harvestingstat.cv", "demandpercapita", "ptosell", "lossfrac",
            "fertility", "yield", "suitability", "transportation", "maintenance",
            "steepness", "floorbiomass",

            "nonlaborcost.mean", "nonlaborcost.cv", "yield.mean", "yield.cv",

            "pricestat.mean", "pricestat.cv",
            "exppayofftoland.agent1", "exppayofftolabor.agent1",
            "exppayofftoland.agent2", "exppayofftolabor.agent2",
            "subsidy_estsub", "subsidy_mntsub", "estcoststat.mean", "estcoststat.cv",
            "estlaborstat.mean", "estlaborstat.cv", "extlaborstat.mean", "extlaborstat.cv",

            "culturaldeliberation", "event", "credibility", "exposure",
            "explabor", "expland", "pfireuse"
            ),
  title = c("Min", "Max", "Mean", "CV", "Depletion Rate",
            "Half time recovery", "Mean", "CV", "Mean", "CV", "Mean", "CV",

            "Mean", "CV", "Demand per capita", "Potential to sell", "Loss fraction",
            "Fertility", "Yield", "Suitability", "Transportation", "Maintenance",
            "Steepness", "Floor biomass",

            "Mean", "CV", "Mean", "CV",

            "Mean", "CV", "Return to land [agent1]", "Return to labor [agent1]",
            "Return to land [agent2]", "Return to labor [agent2]",
            "Plot establishment", "Mnt", "Mean", "CV", "Mean", "CV", "Mean", "CV",

            "Cultural influence (0-1)", "Availability", "Credibility", "Exposure",
            "Expansion of labor", "Expansion of land", "Fire use"
            )

)

############### OTHER ##################
demographics_df <- data.frame(
  field = c("initpop", "annualgrowthrate", "laborfraction", "workingdays",
            "initfinance", "secconsumptionfrac"),
  label = c("Initial population", "Annual growth rate", "Labour fraction",
            "Working days", "Initial financial capital", "Secondary consumption"),
  unit = c("people", "x100%", "0-1", "person-day", "USD", "0-1"),
  value = 0
)

demographics_col_df <- data.frame(
  col = c("label", "unit", "value"),
  label = c("Parameters", "Unit", "Value")
)

agentprop_df <- data.frame(
  field = c("popfraction", "alpha_learning", "beta_learning", "prioritization"),
  label = c("Population fraction", "Alpha factor", "Beta factor", "Landuse priority"),
  unit = rep("0-1", 4),
  value1 = 0,
  value2 = 0
)

agentprop_col_df <- data.frame(
  col = c("label", "unit", "value1", "value2"),
  label = c("Parameters", "Unit", "Type 1", "Type 2")
)

converter_df <- data.frame(
  field = c("vol_to_biomass", "biomass_to_c", "curr_to_usd", "ext_labor_fee"),
  label = c("Timber volume to biomass", "Biomass to carbon",
            "Local currency per USD", "Fee of external abour"),
  unit = c("", "", "", "USD/pd"),
  value = c(1.25, 0.45, 0, 4.29)
)

converter_col_df <- data.frame(
  col = c("label", "unit", "value"),
  label = c("Parameters", "Unit","Value")
)

disaster_df <- data.frame(
  field = c("disaster_human", "disaster_capital", "disaster_work", "disaster_time"),
  label = c("* To human", "* To money capital", "* To working day",
            "Time of disaster event"),
  unit = c("0-1", "0-1", "0-1", "year"),
  value = c(0, 0, 0, 100)
)

disaster_col_df <- data.frame(
  col = c("label", "unit", "value"),
  label = c("Impact of disaster", "Unit", "Value")
)

other_inp_col_list <- list(demographics = demographics_col_df,
                           farmer = agentprop_col_df,
               disaster = disaster_col_df,
               converter = converter_col_df)

other_inp_df <- data.frame(
  id = c("demographics", "farmer", "disaster", "converter"),
  table = c("demographics_df", "agentprop_df", "disaster_df", "converter_df"),
  title = c("Demography", "Farmer learning", "Disaster", "Converter"),
  width = c(6,6,6,6)
)

##############################################################  

#convert lc input to new format
import_old_lc_par <- function(lc_df, merged_lc_df = NULL) {
  df <- merged_lc_df
  
  if(is.null(df)) {
    df <- data.frame(matrix(data=NA, ncol = length(lc_field)))
    names(df) <- lc_field
    df <- df[0,]
  } 
  df$landcover <- NULL
  df <- merge(df, lc_df, by = "lc_id", all = T)
  # print(df)
  lcs <- strsplit(df$landcover, split = "_")
  a <- lapply(lcs, function(x){x[1]})
  b <- lapply(lcs, function(x){ifelse(is.na(x[2]), "", x[2]) })
  c <- lapply(b, match_growth_stage)
  df$landcover <- unlist(a)
  df$growth_stage <- unlist(c)
  
  #apply the land use from LC acronym
  for(i in c(1:4))
    df[grep(landuse_conv[i], df$landcover), "landuse"] <- landuse_list[i]
  df <- df[order(df$lc_id),]
  return(df)
}

combine_lulc <- function(x) {
  ifelse(is.na(x[2]) | x[2] == "", x[1], 
         paste0(x[1], "[", x[2],"]"))
}

generate_param_tables <- function(lc, old_landuse_df = NULL, old_map_data_df = NULL) {
  if(nrow(lc) == 0) return(NULL)
  
  p <- list()
  ############################################
  ## LAND USE ################################
  ############################################
  ## update land use
  lu <- unique(lc[c("landuse", "landcover")])
  lu <- lu[lu$landuse != "" & !is.na(lu$landuse), ]
  if(nrow(lu) == 0) return(NULL)
  
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
  if(is.null(old_landuse_df)) {
    lu$color <- rainbow(nrow(lu))
  } else {
    lu$color <- old_landuse_df$color[1:nrow(lu)]
    n <- length(lu$color[is.na(lu$color) | lu$color == ""])
    lu$color[is.na(lu$color) | lu$color == ""] <- randomColor(n)
  }
  ## update lu_id on lc table
  lc$lulc <- apply(lc[c("landuse", "landcover")], 1, combine_lulc)
  lc$lu_id <- NULL
  lc <- merge(lc, lu[c("lu_id", "landuse", "lu_short")], by.x = "lulc", by.y = "landuse", all.x = T)
  lc <- lc[order(lc$lc_id),]
  
  lc$lc_short <- paste0(lc$lu_short, toupper(substr(lc$growth_stage, 1, 2)))
  p$landuse_df <- lu
  
  ############################################
  ## LIVELIHOOD ##############################
  ############################################
  lu_to_ll <- lu[!lu$lu_group %in% c("Settlement", "Forest"), ]
  forest_lu_id <- min(lu$lu_id[lu$lu_group == "Forest"])
  ll_df <- data.frame(
    livelihood = c(livelihoodtype_base, lu_to_ll$landuse),
    ll_short = c(livelihoodtype_base, lu_to_ll$lu_short),
    lu_id = c(NA, forest_lu_id, forest_lu_id, lu_to_ll$lu_id),
    lu_group = c(livelihoodtype_base_group, lu_to_ll$lu_group))
  ll_df$ll_id = c(1:nrow(ll_df))  
  p$livelihood_df <- ll_df[ll_field]
  
  
  ## update ll_id on lc table
  lc$ll_id <- NULL
  lc <- merge(lc, ll_df[c("ll_id", "livelihood")], by.x = "lulc", by.y = "livelihood", all.x = T)
  p$landcover_df <- lc[lc_field]
  
  ############################################
  ## MAP TABLE ###############################
  ############################################
  if(is.null(old_map_data_df)) {
    map_df <- map_data_df
  } else {
    map_df <- old_map_data_df
  }
  map_df$ll_id <- as.character(map_df$ll_id)
  
  ## update list of suitability map input options  
  ll_suit_df <- ll_df[ll_df$lu_group %in% c("Tree-based system", "Agriculture"), ]
  map_suit_df <- ll_suit_df[c("livelihood", "ll_id")]
  names(map_suit_df) <- c("label", "ll_id")
  map_suit_df$ll_id <- as.character(map_suit_df$ll_id)
  map_suit_df$group <- rep("suitability", nrow(map_suit_df))
  map_suit_df$id <- paste0("suit_",c(1:nrow(map_suit_df)))
  map_suit_df$file <- NA
  map_df$id <- as.character(map_df$id)
  map_df <- map_df[map_df$group != "suitability",]
  map_df <- rows_append(map_df, map_suit_df)
  
  ## update list of proximity map input options
  ll_dist_df <- ll_df[ll_df$lu_group %in% c("Forest", "Tree-based system", "Agriculture"), ]
  lup <- paste("Processing industry of",  ll_dist_df$livelihood)
  map_dist_df <- data.frame(label = c(map_dist_label_base, lup))
  map_dist_df$group <- rep("proximity", nrow(map_dist_df))
  map_dist_df$id <- paste0("proxi_",c(1:nrow(map_dist_df)))
  map_dist_df$file <- NA
  map_dist_df$ll_id <- c(rep("", length(map_dist_label_base)), ll_dist_df$ll_id)
  map_df <- map_df[map_df$group != "proximity",]
  map_df <- rows_append(map_df, map_dist_df)
  
  ids <- map_df$id
  map_df$id <- factor(ids, levels = ids)
  p$map_data_df <- map_df
  
  ###Biophysic input table here #########
  bio <- lc[c("lc_id", "lc_short")]
  bio_id <- par_scalar_title_df$id[par_scalar_title_df$table == "bio_lc_df"]
  bio_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% bio_id]
  lapply(bio_fl, function(x){
    bio[[x]] <<- 0
  })
  p$bio_lc_df <- bio
  
  eco <- lc[c("lc_id", "lc_short")]
  eco_id <- par_scalar_title_df$id[par_scalar_title_df$table == "eco_lc_df"]
  eco_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% eco_id]
  lapply(eco_fl, function(x){
    eco[[x]] <<- 0
  })
  p$eco_lc_df <- eco
  
  ##########################################################
  bio_ll <- ll_df[c("ll_id", "ll_short")]
  bio_id <- par_scalar_title_df$id[par_scalar_title_df$table == "bio_ll_df"]
  bio_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% bio_id]
  lapply(bio_fl, function(x){
    bio_ll[[x]] <<- 0
  })
  p$bio_ll_df <- bio_ll
  
  eco_ll <- ll_df[c("ll_id", "ll_short")]
  eco_id <- par_scalar_title_df$id[par_scalar_title_df$table == "eco_ll_df"]
  eco_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% eco_id]
  lapply(eco_fl, function(x){
    eco_ll[[x]] <<- 0
  })
  p$eco_ll_df <- eco_ll
  
  soc_ll <- ll_df[c("ll_id", "ll_short")]
  soc_id <- par_scalar_title_df$id[par_scalar_title_df$table == "soc_ll_df"]
  soc_fl <- par_scalar_field_df$field[par_scalar_field_df$id %in% soc_id]
  lapply(soc_fl, function(x){
    soc_ll[[x]] <<- 0
  })
  p$soc_ll_df <- soc_ll

  
  #########################################################
  return(p)
}

suit_map_df <- data.frame(
  id = c("food1", "food2", "food3", "food4", "af1", "af2", "af3", "af4", 
         "af5", "af6", "af7", "af8"),
  old_id = c("suitfood1", "suitfood2", "suitfood3", "suitfood4", 
             "suitaf1", "suitaf2", "suitaf3", "suitaf4", "suitaf5", 
             "suitaf6", "suitaf7", "suitaf8"),
  new_id = paste0("suit_", c(1:12))
)

dist_map_df <- data.frame(
  map_type = c("zroad", "zriver", "zmart", "ztimber", "zfd1", 
               "zfd2", "zfd3", "zfd4", "zaf1",
               "zaf2", "zaf3", "zaf4", "zaf5", "zaf6",
               "zaf7", "zaf8", "zntfp", "zset"),
  new_id  = paste0("proxi_", c(1:3, 6:18, 5, 4)),
  fid = c("droada", "drivera", "dmarta", "dindtimbera", "dindfood1a", 
          "dindfood2a", "dindfood3a", "dindfood4a", "dindaf1a",
          "dindaf2a", "dindaf3a", "dindaf4a", "dindaf5a", "dindaf6a",
          "dindaf7a", "dindaf8a", "dindntfpa", "dseta")
)

#############

get_map_color <- function(map) {
  v <- unique(as.vector(map[[1]]))
  v <- v[!is.na(v)]
  n <- min(length(v), 50)
  if(n == 1) pal <- COLOR_LIGHT
  if(n == 2) pal <- c(COLOR_LIGHT, COLOR_DARK)
  if(n > 2) pal <- hcl.colors(n, 'Spectral')
  return(pal)
}

#####################################

params_file_df <- data.frame(
  var = c("landcover_df", "landuse_df", "livelihood_df", "map_data_df",
          "bio_lc_df", "bio_ll_df", "eco_lc_df", "eco_ll_df", "soc_ll_df", 
          "demographics_df", "agentprop_df", "disaster_df", "converter_df"),
  file = c("landcover.csv", "landuse.csv", "livelihood.csv", "map_data.csv", 
           "biophysics_lc.csv", "biophysics_ll.csv", "economics_lc.csv", 
           "economics_ll.csv", "socio_ll.csv", "demographics.csv", 
           "agentprop.csv", "disaster.csv", "converter.csv"),
  label = c("Land cover", "Land use", "Livelihood", "Spatial data", 
            "Biophysics LC", "Biophysics LL", "Economics LC", 
            "Economics LL", "Socio-Cultural", "Demography", 
            "Farmer learning", "Disaster", "Converter")
)

#############################################
### OUTPUT ##################################
#############################################

out_df <- data.frame(
  var = c("supplysufficiency", "price", "buying", "selling", 
          "availablelabor", "availablemoney",
          "potyield", "attyield", "nonlaborcosts", "revenue", "profit",
          "critzonearea",
          "exparealabor", "expareamoney", "exparea", "newplotarea",
          "payofftolabor.agent1", "payofftoland.agent1", "payofftolabor.agent2", 
          "payofftoland.agent2",
          "totsecconsumptionpercapita", "totnetincomepercapita", 
          "totestcost", "totpop", "totagb", "totagc", "firearea"),
  table = c(rep("market_df", 4), "availablelabor_df", "availablemoney_df",
            rep("yield_df", 5),  
            "critzonearea_df", "exparea_df", "exparea_df", "expprob_df",
            "newplotarea_df", rep("exppayoff_df", 4), rep("out_val_df", 7)),
  label = c("Supply sufficiency", "Price", "Buying", "Selling", 
          "Available labor", "Available money",
          "Potential yield", "Attained yield", "Non labor costs", "Revenue", "Profit",
          "Critical zone area",
          "Expansion area by labor", "Expansion area by money", "Expansion area", "New plot area",
          "Payoff to labor [agent1]", "Payoff to land [agent1]", "Payoff to labor [agent2]", 
          "Payoff to land [agent2]",
          "Total secondary consumption per-capita", "Total net income per-capita", 
          "Total establisment cost", "Total population", "Total aboveground biomass", 
          "Total aboveground carbon", "Fire area")
  
)

out_map_df <- data.frame( 
  id = c("lu_map", "lc_map", "soilfert_map", "agbiomass_map", 
         "agcarbon_map", "fire_map", "lcage_map"),
  label = c("Land use", "Land cover", "Soil ferttlity", "Aboveground biomass", 
            "Aboveground carbon", "Fire", "Land cover age")
)

out_scalar_df <- data.frame(
  table = c("out_lc_df", "out_ll_df", "out_val_df"),
  file = c("out_lc.csv", "out_ll.csv", "out_val.csv")
)
  
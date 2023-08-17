require(stars)
require(terra)

#' Calculate Distances
#' 
#' This is a wrap function for gridDistance() {terra} for stars object
#' The function calculates the distance to cells of a SpatRaster 
#' (here a stars raster map) when the path has to go through the centers 
#' of the eight neighboring raster cells.
#'
#' @param map stars raster map
#' @param target value(s) of the cells from which the distance is calculated. If origin=NULL all cells that are not NA are origins
#'
#' @return The distance map of stars object
#' @export
#'
#' @examples
#' 
calcDistance <- function(map, target = NULL) {
  if(is.na(st_crs(map))) st_crs(map) <- "+proj=robin"
  #TODO: catch here if no cells to compute distance to
  if (!is.null(target) & !(T %in% (target %in% map[[1]]))) return(NULL)
  tryCatch( {
    #convert to terra
    sr = as(map, "Raster")
    srt = rast(sr)
    te <- NULL
    # suppressWarnings(te <- gridDistance(srt, origin = origin, omit = omit))
    suppressWarnings(te <- gridDist(srt, target = target))
    #convert back to stars
    return(st_as_stars(te))
  }
  , error = function(e) {}
  )
  return(NULL)
} 

#' Reclassify Map IDs
#'
#' @param map source map
#' @param from list of IDs from the source map 
#' @param to new list of IDs with max length as the 'from' list. The conversion
#' is following the similar list index  
#' @param default the default converted ID and NULL if no changes for unconverted IDs 
#'
#' @return converted map IDs
#' @export
#'
#' @examples
#' 
reclassify_map_old <- function(map, from, to, default = NA) {
  map2 <- map
  if(!is.null(default)) map2[!is.na(map)] <- default
  for(i in 1:length(from)) {
    if(i <= length(to))
      map2[map == from[i]] <- to[i]
  }
  return(map2)
}

map_factor_to_numeric <- function(map) {
  v <- unlist(map[[1]])
  nr <- nrow(map[[1]])
  map[[1]] <- matrix(as.numeric(levels(v))[v], nr)
  return(map)
}

# fromto_df <- mn_df
# map <- mn_map
# fromto_df <- parbio1_df[c("lcid", "depletionrate")]
# map <- lc_map
reclassify_map <- function(map, fromto_df) {
  fromto_df <- fromto_df[order(fromto_df[,1]),]
  from <- as.numeric(as.character(unlist(fromto_df[1])))
  to <- as.numeric(as.character(unlist(fromto_df[2])))
  map2 <- cut(map, c(min(from)-1, from), labels = to)
  # map2[[1]] <- matrix(as.numeric(as.character(unlist(map2[[1]]))),nrow=nrow(map2[[1]]))
  # v <- unlist(map2[[1]])
  # nr <- nrow(map2[[1]])
  # map2[[1]] <- matrix(as.numeric(levels(v))[v], nr)
  map2 <- map_factor_to_numeric(map2)
  return(map2)
} 

## ALTERNATIVE!!
# set.seed(1)
# library(stars)
# 
# rcl <- data.frame(from = c(1,4,7), to = c(3,6,9), becomes = 1:3)
# sr <- stars::st_as_stars(matrix(sample(1:9, 100, replace = TRUE), 10, 10))
# 
# plot(cut(sr, c(0, rcl$to), labels = rcl$becomes))


rnorm_to_map <- function(map, ...) {
  d <- dim(map)
  nrow <- d["y"]
  ncol <- d["x"]
  n <- nrow*ncol
  m <- matrix(eval(rnorm(n, ...), envir=list(x=n)), nrow=nrow, ncol=ncol)
  map2 <- map
  map2[[1]] <- m
  map2[is.na(map)] <- NA
  return(map2)
}

runif_to_map <- function(map, ...) {
  d <- dim(map)
  nrow <- d["y"]
  ncol <- d["x"]
  n <- nrow*ncol
  m <- matrix(eval(runif(n, ...), envir=list(x=n)), nrow=nrow, ncol=ncol)
  map2 <- map
  map2[[1]] <- m
  map2[is.na(map)] <- NA
  return(map2)
}

# rnorm_by_ids <- function(map, ids, means, cvs, min_val = NULL, default_val = NA) {
#   mn_map <- map
#   mn_map <- reclassify_map(mn_map, ids, means, default_val)
#   cv_map <- map
#   if(is.na(default_val)) {
#     cv_map <- reclassify_map(cv_map, ids, cvs, NA)
#   } else {
#     cv_map <- reclassify_map(cv_map, ids, cvs, 0)
#   }
#   nr_map <- rnorm_to_map(map)
#   out_map <- mn_map + nr_map * cv_map * mn_map
#   if(!is.null(min_val)) {
#     out_map[out_map < min_val] <- min_val
#   }
#   return(out_map)
# }

  # mn_map2 <- map
  # mn_map2 <- reclassify_map_old(mn_map2, ids, means, default_val)
  #   cv_map2 <- map
  #   if(is.na(default_val)) {
  #     cv_map2 <- reclassify_map_old(cv_map, ids, cvs, NA)
  #   } else {
  #     cv_map2 <- reclassify_map_old(cv_map, ids, cvs, 0)
  #   }
# map <- initlc_map
# ids <- parbio1_df$lcid
# means <- parbio1_df$initlcage.mean
# cvs <- parbio1_df$initlcage.cv

rnorm_by_ids <- function(map, ids, means, cvs, min_val = NULL, default_val = NA) {
  mn_map <- map
  # ids_all <- as.numeric(as.character(unlist(as.data.frame(table(map))[1])))
  # tab_df <- data.frame(ids = ids_all)
  
  # tab_df <- as.data.frame(table(map))[1]
  # names(tab_df) <- c("ids")
  
  ids_all <- unique(as.vector(map[[1]]))
  ids_all <- ids_all[!is.na(ids_all)]
  tab_df <- data.frame(ids = ids_all)
  mncv_df <- data.frame(ids, means, cvs)
  ids_df <- merge(tab_df, mncv_df, by = "ids", all.x = T)
  
  mn_df <- ids_df[c(1,2)]
  if(!is.na(default_val)) mn_df[is.na(mn_df)] <- default_val
  mn_map <- reclassify_map(mn_map, mn_df)
  
  cv_map <- map
  cv_df <- ids_df[c(1,3)]
  if(!is.na(default_val)) cv_df[is.na(cv_df)] <- 0
  cv_map <- reclassify_map(cv_map, cv_df)
  
  ## if all cv == 0, skip rnorm
  nr_map <- rnorm_to_map(map)
  out_map <- mn_map + nr_map * cv_map * mn_map
  if(!is.null(min_val)) {
    out_map[out_map < min_val] <- min_val
  }
  return(out_map)
}

standardize_map <- function(map) {
  if(is.null(map)) return(NULL)
  if(!is(map, "stars")) return(NA)
  max_val <- max(map[[1]], na.rm = TRUE)
  if(is.na(max_val) || max_val == 0) {
    print(paste("standardize_map.max_val:", max_val))
    m <- map
    m[[1]] <- NA
    return(m)
  }
  return(map/max_val)  
}

summarize_list <- function(df_list) {
  df_out <- data.frame()
  for(df in df_list) {
    if(class(df) != "data.frame") {
      v <- unlist(df_list)
      return(data.frame("value" = v))
    }
    df <- setNames(data.frame(t(df[,-1])), df[,1])
    df_out <- dplyr::bind_rows(df_out, df)
  }
  return(df_out)
}

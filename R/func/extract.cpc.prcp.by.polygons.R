#' Summerize CPC Precipitation data by polygons 
#'
#'
#' @param aoi.object Polygon to extract data
#' @param dates Vector of dates (YYYY-MM-DD) to extract data
#' @param poly.id column name of polygon ids
#' 
#' By: Ryan Miller
#' 
#' Last updated: 27 Jan 2023
#' 

extract.cpc.prcp.by.polygons <- function(aoi.object, poly.id, dates){
  
  require(rnoaa)
  require(progress)
  require(raster)
  require(exactextractr)
  require(stringr)
  
  #--Make storage
  prcp <- list()
  ras <- list()
  
  #--Loop over precip data
  print("Getting precipitation data...")
  
  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent eta: :eta",
    total = length(dates), clear = FALSE, width= 60)
  
  for(i in 1:length(dates)){
    pb$tick()
    
    suppressWarnings(
      prcp[[i]] <- cpc_prcp(date=dates[i], drop_undefined=TRUE)
    )
    
    #Ensure results are lat lon with pm at 180
    x <- prcp[[i]]$lon
    x[x>180] <- x[x>180] - 360
    prcp[[i]]$lon <- x
    
    #Make raster
    p <- st_as_sf(prcp[[i]], coords = c("lon","lat"))
    r <- raster(ncols=length(unique(prcp[[i]]$lon)), nrows=length(unique(prcp[[i]]$lat)),
                xmn=-180, xmx=180, ymn=-90, ymx=90)
    
    ras[[i]] <- rasterize(cbind(prcp[[i]]$lon, prcp[[i]]$lat), r, prcp[[i]]$precip, fun = mean)
    
    #Name layer using date
    names(ras[[i]]) <- paste0("X",dates[i])
  }#END
  
  
  #--Summarize by polygons
  print("Summerizine by polygons...")
  
  out<-list()
  
  for(i in 1:length(ras)){
    mat <- exactextractr::exact_extract(x=ras[[i]], y=aoi.object, include_cols=poly.id)
    vec <- sapply(mat, FUN = function(x){cbind.data.frame(id=unique(x[,poly.id]), value=weighted.mean(x=x$value, w=x$coverage_fraction, na.rm=TRUE))})
    out[[i]] <- cbind.data.frame(date=names(ras[[i]]), t(vec))
  }#END
  
  #--Make output
  out <- Reduce(rbind, out)
  
  out <- cbind.data.frame(date=as.character(out$date), id=as.character(out$id), value=as.numeric(out$value))
  
  colnames(out)[which(colnames(out) %in% "id")] <- poly.id
  
  
  #--Convert dates
  out$date <- gsub(".", "-", str_remove(out$date, pattern="X"), fixed = TRUE)
  
  
  return(out)
}#END



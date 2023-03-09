extract.by.polygons <- function(ras, aoi.object, poly.id, agg.fun='mean', by.polygon=TRUE){
  
  #--Load libraries
  require(exactextractr)
  require(sf)
  require(raster)
  require(tidyr)
  require(terra)
  require(data.table)
  
  #--Compare raster and polygon CRS
  print("Ensuring polygon and raster projections are the same...")
  ras.crs <- sf::st_crs(terra::crs(ras))
  aoi.crs <- sf::st_crs(aoi.object)
  
  #--If different reproject raster to avoid geometry issues with polygons
  if(ras.crs!=aoi.crs){
    #ras <- projectRaster(from=ras, crs=aoi.crs$proj4string, method='ngb')
    aoi.object <- st_transform(aoi.object, crs=st_crs(ras.crs), check=TRUE)
  }
  
  #--Extract by polygon
  print("Extracting and summerizing rasters by polygon...")
  
  #--Process by chunks of raster layers
  if(by.polygon==FALSE){
    
    #--Break up into chunks for processing (10% chunks)
    chunklength=round(nlyr(ras)*.05,0)
    vec <- seq(1,nlyr(ras),1)
    chunk <- split(vec,ceiling(seq_along(vec) / chunklength))
    
    #--Pre-allocate list
    df.extract <- vector(mode = "list", length = length(chunk))
    
    
    #--Loop over chunks
    pb <- progress_bar$new(
      format = "  Processing [:bar] :percent eta: :eta",
      total = length(chunk), clear = FALSE, width= 60)
    
    for(i in 1:length(chunk)){
      pb$tick()
      
      tmp <- subset(ras, subset=chunk[[i]])
      #tmp <- aoi.object[chunk[[i]],]
      
      df.extract[[i]] <- exactextractr::exact_extract(x=tmp, y=aoi.object, append_cols=poly.id, weights='area', fun=agg.fun, progress=FALSE)
    }#END Loop
    
    
    #--Convert to data.tables for speed
    for(i in 1:length(df.extract)){
      setDT(df.extract[[i]])
      #setkey(df.extract[[i]], cols="HUC12")
    }#END
    
    
    #--Convert from wide to long
    for(i in 1:length(df.extract)){
      measure.vars <- names(df.extract[[i]])[names(df.extract[[i]]) != poly.id]
      
      df.extract[[i]] = melt(df.extract[[i]], id.vars = poly.id,
                             measure.vars = measure.vars, variable.name='date')
    }#END
    
    
    #--Collapse into single data table
    print("Prepairing output...")
    #df.long <- do.call(rbind, df.extract)
    df.long <- data.table::rbindlist(df.extract)
    
    
    #--Convert to long format
    #df.long <- tidyr::gather(df.extract, date, value, 2:ncol(df.extract), factor_key=TRUE)
    
    #--Fix dates
    df.long$date <- gsub(paste0(agg.fun,"."), "", df.long$date, fixed = TRUE)
    df.long$date <- gsub(".", "-", df.long$date, fixed = TRUE)
    
  return(df.long)
  }#END Logical
  
  
  #--Process by polygons
  if(by.polygon==TRUE){
    chunklength=nrow(aoi.object)
    vec <- seq(1,nrow(aoi.object),1)
    chunk <- split(vec,ceiling(seq_along(vec) / chunklength))
    
    #--Pre-allocate list
    df.extract <- vector(mode = "list", length = length(chunk))
    
    
    #--Loop over chunks
    pb <- progress_bar$new(
      format = "  Processing [:bar] :percent eta: :eta",
      total = length(chunk), clear = FALSE, width= 60)
    
    for(i in 1:length(chunk)){
      pb$tick()
      
      tmp <- aoi.object[chunk[[i]],]
      
      df.extract[[i]] <- exactextractr::exact_extract(x=ras, y=tmp, append_cols=poly.id, weights='area', fun=agg.fun, progress=FALSE)
    }#END Loop
    
    
    #--Convert to data.tables for speed
    for(i in 1:length(df.extract)){
      setDT(df.extract[[i]])
      #setkey(df.extract[[i]], cols="HUC12")
    }#END
    
    
    #--Convert from wide to long
    for(i in 1:length(df.extract)){
      measure.vars <- names(df.extract[[i]])[names(df.extract[[i]]) != poly.id]
      
      df.extract[[i]] = melt(df.extract[[i]], id.vars = poly.id,
                             measure.vars = measure.vars, variable.name='date')
    }#END
    
  return(df.extract)
  }#END Logical
  
  
}#END Function

#---- END END ----




#' Generate EVI by Polygons
#'
#' Generates summary of EVI values by polygons and year using MODIS
#'
#' @param aoi.abbr State abbreviation
#' @param year.vec vector of years to get data for
#' @param write.file TRUE/FALSE Write data to .csv
#' @param write.path File path location to save file
#' 



extract.modis.evi.polygons <- function(aoi.abbr, polys, year.vec, write.file=FALSE, write.path){
  
  require("exactextractr")
  require("MODIStsp")
  require("raster")
  require("progress")
  require("AOI")
  require("operators")
  
  
  #--FNC - Make Raster Brick from Tiff Files
  make.raster.list <- function(file.names){
    
    pb <- progress_bar$new(total = length(file.names))
    for(i in 1:length(file.names)){
      pb$tick()
      
      if(i==1){r.list<-list()
        r.list[[i]]<-raster(file.names[i])
      }
      if(i>1){r.list[[i]]<-raster(file.names[i])}
    }
    
    #r <- r.list
    r <- raster::stack(r.list)
    names(r)<-seq(1,length(r.list),1)
    
    return(r)
  }#END Function
  
  
  
  #-- Make and set working dir
  working.dir.path<-file.path(tempdir(), "temp")
  dir.create(working.dir.path)
  setwd(working.dir.path)
  here::here(working.dir.path)
  
  
  #-- Set area of interest
  spatial.file.path <- paste0(file.path(working.dir.path,"tmp.poly.shp"))
  
  AOI <- aoi_get(state = aoi.abbr)
  AOI <- st_collection_extract(AOI, "POLYGON")
  
  st_write(AOI, dsn=file.path(working.dir.path), layer="tmp.poly.shp", driver='ESRI Shapefile', delete_layer=TRUE)
  
  
  #--Add unk id to polygons
  polys$unk.id <- seq(1,nrow(polys),1)
  
  
  #--Make time vectors
  month.vec <- c(".01",".02",".03",".04",".05",".06",".07",".08",".09",".10",".11",".12")

  #--Process year and month
  out <- list()
  
  
  #--Loop over month and year
  for(y in 1:length(year.vec)){
    print(paste0("Processing ",year.vec[y],"..."))
    
    for(m in 1:length(month.vec)){
    
      print(paste0("    Downloading ",month.vec[m],"..."))
      
      #-- Get MODIS Landcover data
      MODIStsp(gui             = FALSE,
               out_folder      = working.dir.path,
               out_folder_mod  = working.dir.path,
               selprod         = "Vegetation_Indexes_Monthly_1Km (M*D13A3)",
               bandsel         = "EVI",
               sensor          = "Terra",
               user            = "ryan.miller" ,
               password        = "Earthdata2022!",
               start_date      = paste0(year.vec[y],month.vec[m],".01"), 
               end_date        = paste0(year.vec[y],month.vec[m],".01"), 
               verbose         = FALSE,
               spatmeth        = "file",
               spafile         = dirname(spatial.file.path),
               delete_hdf      = TRUE,
               out_format      = "GTiff",
               ts_format       = "R RasterStack",
               reprocess       = TRUE,
               parallel        = 8,
               output_projsel  = "User Defined",
               output_proj     = 4326
      )
      closeAllConnections()
    
    }#END Month
  
    
    #-- Make Raster List
    print("    Making raster stack...")
    
    data.path<-paste0(normalizePath(working.dir.path),"\\temp\\VI_Monthly_1Km_v6\\EVI")
    file.names<-list.files(data.path, full.names=TRUE, pattern=".tif")
    
    r <- make.raster.list(file.names)
  
    
    #-- Extract data for each polygon
    print("    Summerizing by polygons...")
    
    df <- exactextractr::exact_extract(r, polys)
    
    mean.val <- lapply(df, FUN=function(x){mean(as.matrix(x[,1:ncol(x)-1]), na.rm=TRUE)} )
    med.val <- lapply(df, FUN=function(x){median(as.matrix(x[,1:ncol(x)-1]), na.rm=TRUE)} )
    sd.val <- lapply(df, FUN=function(x){sd(as.matrix(x[,1:ncol(x)-1]), na.rm=TRUE)} )
    min.val <- lapply(df, FUN=function(x){min(as.matrix(x[,1:ncol(x)-1]), na.rm=TRUE)} )
    max.val <- lapply(df, FUN=function(x){max(as.matrix(x[,1:ncol(x)-1]), na.rm=TRUE)} )
    
    mean.val <- Reduce(rbind.data.frame, mean.val)
    med.val <- Reduce(rbind.data.frame, med.val)
    sd.val <- Reduce(rbind.data.frame, sd.val)
    min.val <- Reduce(rbind.data.frame, min.val)
    max.val <- Reduce(rbind.data.frame, max.val)
    
    tmp<-Reduce(cbind.data.frame, list(mean.val,med.val,sd.val,min.val,max.val))
    colnames(tmp)<-c("mean","median","sd","min","max") 
    
    tmp<-cbind.data.frame(unk.id=seq(1,nrow(tmp),1), year=year.vec[y],tmp)
    
    tmp<-merge(polys,tmp, by="unk.id",all.x=TRUE)

    out[[y]] <- tmp
   
    
    #--Remove files
    print("Finished processing all years...")
    unlink(file.names)
    
  }#END Year
    
  
  #--Remove geometry
  for(i in 1:length(out)){
    st_geometry(out[[i]]) <- NULL
  }
  
  
  #-- Make final output
  df <- do.call(rbind.data.frame, out)
  
  
  #-- Write File
  if(write.file){
    write.csv(df, paste0(write.path, "evi.", state.abbr, ".", Sys.Date(), ".csv"), row.names=FALSE)
  }
  
  
  #--Remove files
  unlink(working.dir.path, recursive=TRUE)
  
  
  return(df)
}#END Function

#---- END END ----



    






    
    
    
    
    
    
    
    
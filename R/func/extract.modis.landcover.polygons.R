

#' Generate amount of landcover by Counties
#'
#' Generates amount of landcover by county and year using MODIS
#'
#' @param state.abbr State abbreviation
#' @param start.year Start year
#' @param end.year End year
#' @param make.proportion TRUE/FALSE to provide output in proportions
#' @param write.file TRUE/FALSE Write data to .csv
#' @param write.path File path location to save file
#' 


extract.modis.landcover.polygons <- function(aoi.abbr, polys, start.year, end.year, make.proportion=TRUE, write.file=FALSE, write.path){
  
  require("exactextractr")
  require("MODIStsp")
  require("raster")
  require("progress")
  require("AOI")
  require("operators")
  
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
  
  
  #-- Get MODIS Landcover data
  print("Downloading modis layers...")
  
  MODIStsp(gui             = FALSE,
           out_folder      = working.dir.path,
           out_folder_mod  = working.dir.path,
           selprod         = "LandCover_Type_Yearly_500m (MCD12Q1)",
           bandsel         = "LC1",
           sensor          = "Terra",
           user            = "ryan.miller" ,
           password        = "Earthdata2022!",
           start_date      = paste0(start.year,".01.01"), 
           end_date        = paste0(end.year,".12.31"), 
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
  
  
  #-- Make Raster Brick
  print("Making raster brick from modis layers...")
  
  data.path<-paste0(normalizePath(working.dir.path),"\\temp\\LandCover_Type_Yearly_500m_v6\\LC1")
  file.names<-list.files(data.path, full.names=TRUE, pattern=".tif")
  
  pb <- progress_bar$new(total = length(file.names))
  
  for(i in 1:length(file.names)){
    pb$tick()
    
    if(i==1){r.list<-list()
    r.list[[i]]<-raster(file.names[i])
    }
    if(i>1){r.list[[i]]<-raster(file.names[i])}
  }
  
  
  par(mfrow=c(5,4))
  for(i in 1:length(r.list)){
    plot(r.list[[i]])
    
  }
  
  r <- r.list
  
  #r <- brick(r.list)
  
  year.vec <- seq(start.year,end.year,1)
  names(r) <- year.vec
  
  #-- END Make Raster Brick
  
  
  
  #-- Make LUT for Landcover
  print("Assigning landcover classes...")
  
  modis.classes <- c( "Evergreen needleleaf forests",
                      "Evergreen broadleaf forests",
                      "Deciduous needleleaf forests",
                      "Deciduous broadleaf forests",
                      "Mixed forests",
                      "Closed shrublands",
                      "Open shrublands",
                      "Woody savannas",
                      "Savannas",
                      "Grasslands",
                      "Permanent wetlands",
                      "Croplands",
                      "Urban and built-up lands",
                      "Cropland/natural vegetation mosaics",
                      "Snow and ice",
                      "Barren",
                      "Water bodies")
  
  class.lut<-data.frame(land.cat=seq(1,length(modis.classes),1),
                        lc.name=modis.classes)
  
  class.lut[class.lut$land.cat %in% c(1,2,3,4,5),"gen.cat"] <- "forest"
  class.lut[class.lut$land.cat %in% c(6,7,8,9,16),"gen.cat"] <- "Woody_savannas"
  class.lut[class.lut$land.cat %in% c(10),"gen.cat"] <- "grassland"
  class.lut[class.lut$land.cat %in% c(11,17),"gen.cat"] <- "wetland"
  class.lut[class.lut$land.cat %in% c(12,14),"gen.cat"] <- "agricultural"
  class.lut[class.lut$land.cat %in% c(13),"gen.cat"] <- "developed"
  class.lut[class.lut$land.cat %in% c(15),"gen.cat"] <- "snow"
  
  #-- END Make LUT for Landcover
  
  
  #-- Extract landcover values by polygon
  print("Extracting raster values by polygons...")
  
  out <- list()
  
  pb <- progress_bar$new(total = length(r))
  
  for(i in 1:length(r)){
    pb$tick()
    
    tmp<-ratify(r[[i]])
    
    df <- exactextractr::exact_extract(tmp, polys, include_cell=TRUE)
    
    for(j in 1:length(df)){
      cnt <- plyr::count(df[[j]][,c("value")])
      cnt$prop <- cnt$freq / nrow(df[[j]])
      cnt<-data.frame(id=rep(j,nrow(cnt)),
                      land.cat=cnt$x,
                      count=cnt$freq,
                      proportion=cnt$prop)
      levels(cnt$land.cat) <- as.character(seq(1,length(modis.classes),1))
      df[[j]] <- cnt
    }#END
    
    cat.df <- do.call(rbind.data.frame, df)
    cat.df<-merge(cat.df, class.lut, by="land.cat", all.x=TRUE)
    cat.df<-aggregate(count~id+gen.cat, data=cat.df, FUN=sum)
    
    tmp <- tidyr::spread(cat.df[,c("id","gen.cat","count")], gen.cat, count)
    
    tmp[is.na(tmp)==TRUE] <- 0
    
    #col.names<-as.character(seq(1,17,1))
    #missing.col<-col.names[col.names %!in% names(tmp[,-1])] 
    #missing.mat<-data.frame(matrix(0, ncol=length(missing.col),nrow=nrow(tmp)))
    #colnames(missing.mat)<-missing.col
    #tmp<-cbind.data.frame(tmp,missing.mat)
    #tmp<-tmp[,c("id",col.names)]
    
    tmp<-merge(polys,tmp, by.x="OBJECTID", by.y="id",all.x=TRUE)
    
    #year.val<-str_remove(names(r[[i]]), pattern="MCD12Q1_LC1_")
    #year.val<-str_remove(year.val, pattern="_001")
    
    tmp$year<-as.numeric(names(r[i]))
    
    out[[i]] <- tmp
  }
  
  #-- END Extract landcover values by polygon
  
  
  #--Remove geometry
  for(i in 1:length(out)){
    st_geometry(out[[i]]) <- NULL
  }
  
  
  #-- Make final output
  cat.df <- do.call(rbind.data.frame, out)
  
  
  #-- Make Proportional
  if(make.proportion){
    col.names<-colnames(cat.df)[colnames(cat.df) %in%  unique(class.lut$gen.cat)]
    
    col.names<-col.names[col.names %!in% colnames(polys)]
    
    cat.df[,col.names] <- cat.df[,col.names]/rowSums(cat.df[,col.names], na.rm = TRUE)
  }
  
  
  #-- Write File
  if(write.file){
    write.csv(cat.df, paste0(write.path, "landcover.", state.abbr, ".", Sys.Date(), ".csv"), row.names=FALSE)
  }
  
  unlink(working.dir.path, recursive=TRUE)
  
  return(cat.df)
}#END Function

#---- END END ----





    
    
    
    
    
    
    
    
    
#' Summarize terraclim to polygons
#'
#' Generates mean, median, and stdev for polygons using terraclima data
#'
#' @param aoi.value area of interest
#' @param aoi.object polygons representing area of interest
#' @param by.month summerize by month
#' @param year.vec vector of years to get data for
#' @param parameter to extract data for
#' @param polygons to summerize data to
#' @param write file to .csv
#' @param write path
#' 
#' 


extract.terraclim.by.polygons <- function(aoi.value=NULL, aoi.object, by.month=FALSE, year.vec, param.value, polys, write.file=FALSE, write.path){
  
  #--Load and install libraries
  print("Checking for and loading needed libraries...")
  
  packages = c("remotes","tigris", "raster", "exactextractr","progress")
  
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
      }
      library(x, character.only = TRUE)
    }
  )
  
  packages = c("climateR", "AOI")
  
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        remotes::install_github("mikejohnson51/AOI") # suggested!
        remotes::install_github("mikejohnson51/climateR")
        library(x, character.only = TRUE)
      }
    }
  )
  
  #--END Load Libraries
  
  
  #--Get polygons
  #polys<-tigris::counties(state=state.abbr, cb=TRUE, resolution="500k")
  
  
  #--Set area of interest
  if(is.null(aoi.value)==FALSE){
    
    if(aoi.value=="US"){
      AOI = aoi_get(country = aoi.value)
    }
    if(aoi.value=="conus"){
      AOI = aoi_get(state = "conus")
    }
  
  }
  
  if(is.null(aoi.value)==TRUE){
    AOI <- aoi.object
  }
  
  
  #--Years
  #year.vec<-seq(start.year, end.year,1)
  
  
  #--Progress
  pb <- progress_bar$new(total = length(year.vec))
  
  if(by.month==FALSE){

  #--Loop over years
  for(i in 1:length(year.vec)){
    pb$tick()
    
    #--Get Terra Clim
    p<-getTerraClim(AOI=AOI, param=param.value,
                  startDate=paste0(year.vec[i],"-01-01"),
                  endDate=paste0(year.vec[i],"-12-31"))
    closeAllConnections()
    
    #--Convert to raster brick
    r = raster::brick(p)
    
    if(i==1){
        r.mean<-brick(calc(r, mean))
        r.med<-brick(calc(r, median))
        r.sd<-brick(calc(r, sd))
        r.range<-brick(calc(r, max)-calc(r, min))
    }
    if(i>1){
        r.mean[[i]]<-calc(r, mean)
        r.med[[i]]<-brick(calc(r, median))
        r.sd[[i]]<-calc(r, sd)
        r.range[[i]]<-brick(calc(r, max)-calc(r, min))
    }
  }#END Loop
  
    #--Assign layer names
  names(r.mean)<-year.vec
  names(r.med)<-year.vec
  names(r.sd)<-year.vec
  names(r.range)<-year.vec
  
  #--Extract and summarize data by polygons
  
  #Mean
  polys.mu<-exact_extract(r.mean, polys, 'mean')
  names(polys.mu)<-year.vec
  
  #Median
  polys.med<-exact_extract(r.med, polys, 'median')
  names(polys.med)<-year.vec
  
  #StDev
  polys.sd<-exact_extract(r.sd, polys, 'mean')
  names(polys.sd)<-year.vec
  
  #Range
  polys.range<-exact_extract(r.range, polys, 'mean')
  names(polys.range)<-year.vec
  
  
  #--Merge summary stats to polygons
  
  #Mean
  tmp<-cbind.data.frame(polys, polys.mu)
  polys.mu <- tidyr::gather(tmp, year, value, names(polys.mu)[1]:names(polys.mu)[ncol(polys.mu)], factor_key=TRUE)
  polys.mu <- st_drop_geometry(polys.mu)
  polys.mu$statistic <- "mean"
  
  #Median
  tmp<-cbind.data.frame(polys, polys.med)
  polys.med <- tidyr::gather(tmp, year, value, names(polys.med)[1]:names(polys.med)[ncol(polys.med)], factor_key=TRUE)
  polys.med <- st_drop_geometry(polys.med)
  polys.med$statistic <- "median"
  
  #StDev
  tmp<-cbind.data.frame(polys, polys.sd)
  polys.sd <- tidyr::gather(tmp, year, value, names(polys.sd)[1]:names(polys.sd)[ncol(polys.sd)], factor_key=TRUE)
  polys.sd <- st_drop_geometry(polys.sd)
  polys.sd$statistic <- "sd"
  
  #Range
  tmp<-cbind.data.frame(polys, polys.range)
  polys.range <- tidyr::gather(tmp, year, value, names(polys.range)[1]:names(polys.range)[ncol(polys.range)], factor_key=TRUE)
  polys.range <- st_drop_geometry(polys.range)
  polys.range$statistic <- "range"
  
  #--Make final output dataframe
  out<-rbind.data.frame(polys.mu,polys.med,polys.sd,polys.range)
  out$measurement<-"param.value"
  
  }#END by year
  
  
  if(by.month==TRUE){
    
    p<-getTerraClim(AOI=AOI, param=param.value,
                    startDate=paste0(year.vec[1],"-01-01"),
                    endDate=paste0(year.vec[length(year.vec)],"-12-31"))
    
    #--Convert to raster brick
    r = raster::brick(p)
      
    #--Extract and summarize data by polygons
    
    #Mean
    polys.mu<-exact_extract(r, polys, 'mean')

    #Median
    polys.med<-exact_extract(r, polys, 'median')

    #StDev
    polys.sd<-exact_extract(r, polys, 'stdev')

    #Min
    polys.min<-exact_extract(r, polys, 'min')

    #Max
    polys.max<-exact_extract(r, polys, 'max')
    
    #--Merge summary stats to polygons
    
    #Mean
    tmp<-cbind.data.frame(polys, polys.mu)
    polys.mu <- tidyr::gather(tmp, year, value, names(polys.mu)[1]:names(polys.mu)[ncol(polys.mu)], factor_key=TRUE)
    polys.mu <- st_drop_geometry(polys.mu)
    polys.mu$statistic <- "mean"
    
    #Median
    tmp<-cbind.data.frame(polys, polys.med)
    polys.med <- tidyr::gather(tmp, year, value, names(polys.med)[1]:names(polys.med)[ncol(polys.med)], factor_key=TRUE)
    polys.med <- st_drop_geometry(polys.med)
    polys.med$statistic <- "median"
    
    #StDev
    tmp<-cbind.data.frame(polys, polys.sd)
    polys.sd <- tidyr::gather(tmp, year, value, names(polys.sd)[1]:names(polys.sd)[ncol(polys.sd)], factor_key=TRUE)
    polys.sd <- st_drop_geometry(polys.sd)
    polys.sd$statistic <- "sd"
    
    #Min
    tmp<-cbind.data.frame(polys, polys.min)
    polys.min <- tidyr::gather(tmp, year, value, names(polys.min)[1]:names(polys.min)[ncol(polys.min)], factor_key=TRUE)
    polys.min <- st_drop_geometry(polys.min)
    polys.min$statistic <- "min"
    
    #Max
    tmp<-cbind.data.frame(polys, polys.max)
    polys.max <- tidyr::gather(tmp, year, value, names(polys.max)[1]:names(polys.max)[ncol(polys.max)], factor_key=TRUE)
    polys.max <- st_drop_geometry(polys.max)
    polys.max$statistic <- "max"
    
    #--Make final output dataframe
    out<-rbind.data.frame(polys.mu,polys.med,polys.sd,polys.min,polys.max)
    out$measurement<-param.value
    
  }#END by month
  
  
  
  #-- Write File
  if(write.file){
    write.csv(out, paste0(write.path, param.value, ".", aoi.value, ".", start.year, ".", end.year, ".", Sys.Date(), ".csv"), row.names=FALSE)
  }
  
  
  return(out)
}#END 

#---- END END ----  











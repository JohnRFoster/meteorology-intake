#' Summarize gridmet by polygons and time period
#'
#' Generates mean, median, and stdev for counties using gridMet data
#'
#' @param aoi.object Polygon to extract data
#' @param str.date Start date
#' @param end.date End date
#' @param param.value parameter to extract
#' 
#' 

extract.daymet.by.polygons.time.varying <- function(aoi.object, str.date, end.date, param.value){
  
  #--Load and install libraries
  require(raster)
  require(progress)
  require(climateR)
  require(AOI)
  require(FedData)
  
  #--Ensure geometries are valid
  
  # print("Ensuring geometries are valid...")
  # 
  # if(all(st_is_valid(aoi.object))==FALSE){
  #   aoi.object <- st_make_valid(aoi.object)
  #   aoi.object <- st_buffer(aoi.object, 0.0)
  # }
  
  #--END Ensure geometries valid
  
  #--Get GridMet
  suppressMessages(
    
  #--Use ClimateR (issues with current function)
  # p<-getDaymet(AOI=aoi.object, param=param.value,
  #                startDate=str.date,
  #                endDate=end.date)
  # 
  # )
    
  #--Use FedData package
  p <- FedData::get_daymet(template=as(aoi.object, "Spatial"),
                           label="x",
                           elements=param.value,
                           years=year(str.date):year(end.date),
                           tempo="day")
  )
  
  closeAllConnections()
  
  #--Limit to those dates to keep
  days.keep <- paste0("X",seq(as.Date(str.date,"%Y-%m-%d"),as.Date(end.date,"%Y-%m-%d"),1))
  days.keep <- gsub("-", ".", days.keep, fixed = TRUE)
  
  p <- subset(p[[1]], subset=days.keep)
  
  #--Reproject to match aoi.object
  p <- projectRaster(p, crs=st_crs(aoi.object)$proj4string)
  
  #--Ensure no negative values resulting from transformation (prcp only)
  if(param.value=="prcp"){
   p[p<0] <- 0 
  }
  
  #--Mask raters to polygon
  p <- mask(p, mask=aoi.object, updatevalue=NA)
  
  #--Convert to raster brick
  #r = raster::brick(p)
    
  #--Convert to array
  r = getValues(p)
    
  #--Generate summary stats
  r.mean<-mean(r,na.rm=TRUE)
  r.med<-median(r,na.rm=TRUE)
  r.sd<-sd(r,na.rm=TRUE)
  r.max<-max(r,na.rm=TRUE)
  r.min<-min(r,na.rm=TRUE)
  r.range<-max(r,na.rm=TRUE)-min(r,na.rm=TRUE)

  out <- cbind.data.frame(mean=r.mean,
                          median=r.med,
                          sd=r.sd,
                          min=r.min,
                          max=r.max,
                          range=r.range)
  
  if(param.value=="prcp"){

    #--Total prcp over period
    r.sum <- calc(p, sum, na.rm=TRUE)
    r.total <- mean(getValues(r.sum), na.rm=TRUE)
    
    #--Number of days with rain fall
    p.binary <- p
    p.binary[p.binary>0]<-1
    
    r.days <- calc(p.binary, sum, na.rm=TRUE)
    r.days <- mean(getValues(r.days), na.rm=TRUE)
    
    
    out <- cbind.data.frame(total=r.total,
                            num.days=r.days,
                            mean.daily=r.mean,
                            median.daily=r.med,
                            sd.daily=r.sd,
                            min.daily=r.min,
                            max.daily=r.max,
                            range.daily=r.range)
  }
  
  return(out)
}#END 

#---- END END ----  











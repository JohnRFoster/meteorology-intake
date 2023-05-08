
#' Generate standardized precipitation index using gridmet precipitation data for a polygon 
#'
#' Generates time series of mean daily precipitation for a polygon.  This is used to calculate
#' the daily standardized precipitation index for a 30 day (spi-1) and 90 day (spi-3) period using
#' the entire reference data.
#'
#' @param aoi.object Polygon to extract data
#' @param str.date Start date
#' @param end.date End date
#' @param str.re.date start date for reference period - by default is set to str.date-(365*21)
#' @param end.re.date end date for reference period - by default is set to Sys.Date()
#' 
#' By: Ryan Miller
#' 
#' Last updated: 26 Jan 2023
#' 

make.stand.precip.index <- function(aoi.object, str.date, end.date, str.ref.date=as.Date(str.date)-365*21, end.ref.date=Sys.Date()){
  
  #--Load and install libraries
  require(raster)
  require(progress)
  require(climateR)
  require(AOI)
  require(FedData)
  library(standaRdized)
  
  #--Ensure geometries are valid
  
  # print("Ensuring geometries are valid...")
  # 
  # if(all(st_is_valid(aoi.object))==FALSE){
  #   aoi.object <- st_make_valid(aoi.object)
  #   aoi.object <- st_buffer(aoi.object, 0.0)
  # }
  
  #--END Ensure geometries valid
  
  print("Getting precipitation data...")
  
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
                           elements="prcp",
                           years=year(str.ref.date):year(end.ref.date),
                           tempo="day")
  )
  
  closeAllConnections()
  
  
  #--Limit to those dates to keep
  days.keep <- paste0("X",seq(as.Date(str.ref.date,"%Y-%m-%d"),as.Date(end.ref.date,"%Y-%m-%d"),1))
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
  

  #--Make time series
  print("Generating average daily precipitation for polygon...")
  vec <- vector()
  
  pb <- progress_bar$new(total = nlayers(p))
  for(i in 1:nlayers(p)){
    pb$tick()
    
    vec[i] <- mean(getValues(p[[i]]), na.rm=TRUE)
  }
  
  time.series<-as.matrix(vec, ncol=1)
  
  rownames(time.series) <- gsub(".", "-", str_remove(names(p), pattern="X"), fixed = TRUE)
  colnames(time.series) <- "value"
  
  time.series <- as.xts(time.series)

  
  print("Generating average daily precip for polygon...")
  
  #--Make dates to calculate SPI
  dates <- seq(as.Date(str.date,"%Y-%m-%d"),as.Date(end.date,"%Y-%m-%d"),1)
  
  #--Generate SPI
  print("Generating indexes for polygon...")
  
  spi.1 <- standardized.index(data=time.series, agg.length = 30, index.out = dates)

  spi.3 <- standardized.index(data=time.series, agg.length = 90, index.out = dates)
  
  spi.index <- cbind.data.frame(date=index(spi.1), spi.1=spi.1$value, spi.3=spi.3$value)
  
  rownames(spi.index) <- seq(1, nrow(spi.index),1)
  
return(spi.index)
}#END

#---- END END ----







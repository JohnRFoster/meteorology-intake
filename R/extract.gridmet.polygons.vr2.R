#' Summarize gridmet data by polygons
#'
#' Generates mean, median, and stdev for polygons using gridMet data
#'
#' @param aoi.value area of interest
#' @param year.vec vector of years
#' @param param.value gridmet parameter to extract
#' @param polygons to summarize data with
#' @param write file to .csv
#' @param write path
#' 
#' 


extract.gridmet.by.polygons <- function(aoi.value, year.vec, param.value, polys, write.file=FALSE, write.path){

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
if(aoi.value=="US"){
  AOI = aoi_get(country = aoi.value)
}
if(aoi.value=="conus"){
  AOI = aoi_get(state = "conus")
}

#--Years
#year.vec<-seq(start.year, end.year,1)


#--Progress
pb <- progress_bar$new(total = length(year.vec))

#--Loop over years
for(i in 1:length(year.vec)){
  pb$tick()
  
  #--Get GridMet
  p<-getGridMET(AOI=AOI, param=param.value,
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

#-- Write File
if(write.file){
  write.csv(out, paste0(write.path, param.value, ".", aoi.value, ".", start.year, ".", end.year, ".", Sys.Date(), ".csv"), row.names=FALSE)
}


return(out)
}#END 

#---- END END ----  











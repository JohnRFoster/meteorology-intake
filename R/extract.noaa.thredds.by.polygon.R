



#---- Load Libraries ----

library(landscapemetrics)
library(landscapetools)
library(MODIStsp)
library(sf)
library(AOI)
library(progress)
library(raster)
library(stringr)
library(imputeTS)

#---- END ----



#---- Source Functions ----

source("R/ClimateSpatialR/extract.gridmet.polygons.vr2.R")
source("R/ClimateSpatialR/extract.modis.landcover.polygons.R")
source("R/ClimateSpatialR/extract.modis.evi.polygons.R")
source("R/ClimateSpatialR/extract.modis.snow.polygons.R")
source("R/ClimateSpatialR/extract.terraclim.by.polygons.R")
source("R/GeoData.Functions/extract.geodata.human.pop.polygons.R")
source("R/ClimateSpatialR/extract.gridmet.by.polygons.time.varying.R")
source("R/ClimateSpatialR/extract.cpc.prcp.by.polygons.R")

#---- END ----



#---- Set Paths ----

sp.path <- "C:/DATA/Watersheds/"
write.path <- "C:/Documents/Project Documents/AI Surveillance Planning 2023/Data/"

#---- END ----



#---- Read Polygon Data ----

sp.path <- "C:/DATA/Watersheds/"

polys <- st_read(dsn=file.path(sp.path), layer="huc12.watersheds")

#---- END ----


















library(raster)
library(ncdf4)

the_NetCDF <- tempfile(fileext = ".nc")

download.file("https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/Dailies/monolevel/air.2m.2005.nc", the_NetCDF, mode = "wb")


our_nc_data <- nc_open(the_NetCDF)


library(raster)
temp = stack(the_NetCDF)



https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/Dailies/monolevel/air.2m.2005.nc



thredds.path <- "https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/Dailies/monolevel/"

product.name <- "air.2m."
product.name <- "apcp."

str.year = 1980
end.year =2022


download.noaa.thredds.data <- function(product.name, thredds.path, str.year, end.year){

  #--Load libraries
  require(utils)
  require(raster)

  #Make vector of years to download
  years <- seq(str.year, end.year, 1)

  #Make Storage
  ras <- list()

  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent eta: :eta",
    total = length(years), clear = FALSE, width= 60)

  #Loop over years to download
  for(i in 1:length(years)){
    pb$tick()

    #Make file name
    file.name <- file.path(thredds.path, paste0(product.name, years[i],".nc"))

    #Make temp file name
    temp.file <- tempfile(fileext = ".nc")

    #Download file
    download.file(file.name, temp.file, mode = "wb", quiet=TRUE)

    #Read netcdf file using raster::stack into list
    ras[[i]] <- stack(temp.file)

  }#END Loop

  ras <- stack(ras)

return(ras)
}#END Function




#--FNC - Extract mean from raster stack by polygons
extract.mean.by.polygons <- function(ras, aoi.object, poly.id){

  #--Load libraries
  require(exactextractr)
  require(sf)
  require(raster)
  require(tidyr)

  #--Compare raster and polygon CRS
  print("Ensuring polygon and raster projections are the same...")
  ras.crs <- sf::st_crs(raster::crs(ras))
  aoi.crs <- sf::st_crs(aoi.object)

  #--If different reproject raster to avoid geometry issues with polygons
  if(ras.crs!=aoi.crs){
    ras <- projectRaster(from=ras, crs=aoi.crs$proj4string, method='ngb')
  }

  #--Extract by polygon
  print("Extracting and summerizing rasters by polygon...")
  df.extract <- exactextractr::exact_extract(x=ras, y=aoi.object, append_cols=poly.id, weights='area', fun='mean')

  #--Convert to long format
  print("Prepairing output...")
  df.long <- tidyr::gather(df.extract, date, value, 2:ncol(df.extract), factor_key=TRUE)

  #--Fix dates
  df.long$date <- gsub("mean.X", "", df.long$date, fixed = TRUE)
  df.long$date <- gsub(".", "-", df.long$date, fixed = TRUE)

return(df.long)
}#END Function






#--Download noaa data
ras <- download.noaa.thredds.data(product.name, thredds.path, str.year, end.year)


#--Extract mean by polygons
out <- extract.mean.by.polygons(ras, aoi.object, poly.id)












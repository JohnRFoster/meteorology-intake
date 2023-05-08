

options(scipen=999)

#---- Refreash session if needed ----
rm(list = ls())
gc()
# .rs.restartR()
#---- END ----



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
library(data.table)
library(elevatr)

#---- END ----



#---- Source Functions ----

source("R/func/extract.gridmet.polygons.vr2.R")
source("R/func/extract.modis.landcover.polygons.R")
source("R/func/extract.modis.evi.polygons.R")
source("R/func/extract.modis.snow.polygons.R")
source("R/func/extract.terraclim.by.polygons.R")
source("R/func/extract.cpc.prcp.by.polygons.R")
source("R/func/download.noaa.thredds.data.R")
source("R/func/extract.by.polygons.R")
# source("R/func/extract.geodata.human.pop.polygons.R") # don't have
# source("R/func/extract.gridmet.by.polygons.time.varying.R") # don't have
# source("R/func/Repair.Geometry.Tools.R") # don't have
#---- END ----


#---- Other Functions ----

#--FNC - Reload downloaded NOAA data
load.downloaded.data <- function(data.path){

  file.list <- list.files(data.path, pattern=".nc", full.names = TRUE)

  ras<-list()

  for(i in 1:length(file.list)){
    tmp <- terra::rast(file.list[i])
    names(tmp) <- as.character(terra::time(tmp))

    ras[[i]] <- tmp
  }

  ras <- terra::rast(ras)

  return(ras)
}#--END Function

#---- END Functions ----





#---- Set Paths ----

sp.path <- "data/Cartography.Layers/"
write.path <- "data/"

#---- END ----



#---- Read Polygon Data ----

polys <- st_read(dsn=file.path(sp.path), layer="Counties_dtl_geo")
colnames(polys) <- tolower(colnames(polys))

polys <- polys[,c("name","state_name","state_fips","cnty_fips","fips")]

#---- END ----



#---- Make AOI Data ----

aoi.object <- st_read(dsn=file.path("C:/DATA/Cartography.Layers/"), layer="All.Admin")

#---- END ----



#---- Read Point Level Sample Data ----

#--Set Paths
data.path<-"C:/DATA/DART/IAV/"

#--Read data
pnt.dat<-fread(paste0(data.path, "IAV.Cleaned_Apr2007_to_Jan2023.huc12.2023-02-07.csv"))

#--Ensure watersheds are character
pnt.dat$huc12 <- as.character(pnt.dat$huc12)
pnt.dat[nchar(pnt.dat$huc12)==10,"huc12"] <- paste0("00",pnt.dat[nchar(pnt.dat$huc12)==10,"huc12"])
pnt.dat[nchar(pnt.dat$huc12)==11,"huc12"] <- paste0("0",pnt.dat[nchar(pnt.dat$huc12)==11,"huc12"])

#--Make unique watershed / date combinations
aoi.date.poly <- unique(pnt.dat[,c("date_collected","huc12")])

#--Link sampling date to polygons
tmp<-list()

#--Loop over unique date / watershed combinations
pb <- progress_bar$new(
  format = "  Processing [:bar] :percent eta: :eta",
  total = nrow(aoi.date.poly), clear = FALSE, width= 60)

for(i in 1:nrow(aoi.date.poly)){
  pb$tick()

  tmp[[i]] <- merge(polys, aoi.date.poly[i,], by.x="HUC12", by.y="huc12")
}#END

#--Make single polygon file
aoi.date.poly <- dplyr::bind_rows(tmp)

#--Convert to date
aoi.date.poly$date_collected <- as.Date(aoi.date.poly$date_collected, "%Y-%m-%d")

#---- END ----




#---- TMIN, TMAX, Solar Radiation (time varying, 30 days prior to sample) ----

#--FNC - Generate temperature data
generate.temp.data <- function(aoi.object, param.value){

  out <- list()

  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent eta: :eta",
    total = nrow(aoi.object), clear = FALSE, width= 60)

  for(i in 1:nrow(aoi.object)){
    pb$tick()

    str.date <- aoi.object[i,"date.collected"] - 30
    str.date <- str.date$date.collected

    end.date <- aoi.object[i,"date.collected"]
    end.date <- end.date$date.collected

    aoi.obj=aoi.object[i,]

    out[[i]] <- extract.daymet.by.polygons.time.varying(aoi.obj, str.date, end.date, param.value=param.value)
    #out[[i]] <- extract.gridmet.by.polygons.time.varying(aoi.object, str.date, end.date, param.value=param.value)
  }

  out <- Reduce(rbind, out)

  return(out)
}#END Function



#--Generate TMAX by day (degrees Celsius)
dat.tmax <- generate.temp.data(aoi.object=aoi.date.poly, param.value="tmax")
colnames(dat.tmax) <- paste0("tmax.", colnames(dat.tmax))


#--Generate TMIN by day (degrees Celsius)
dat.tmin <- generate.temp.data(aoi.object=aoi.date.poly, param.value="tmin")
colnames(dat.tmin) <- paste0("tmin.", colnames(dat.tmin))

temp.dat <- cbind.data.frame(dat.tmin, dat.tmax)

#--Generate ranges for Temperature
temp.dat$mean.range <- temp.dat$tmax.mean - temp.dat$tmin.mean
temp.dat$median.range <- temp.dat$tmax.median - temp.dat$tmin.median
temp.dat$extremes.range <- temp.dat$tmax.max - temp.dat$tmin.min


#--Generate Precipitation
dat.prcp <- generate.temp.data(aoi.object=aoi.date.poly, param.value="prcp")
colnames(dat.prcp) <- paste0("prcp.", colnames(dat.prcp))


#--Generate solar radiation by day (Incident shortwave radiation flux density in watts per square meter)
dat.srad <- generate.temp.data(aoi.object=aoi.date.poly, param.value="srad")
colnames(dat.srad) <- paste0("srad.", colnames(dat.srad))


#--Merge into single dataframe
out <- Reduce(cbind.data.frame, list(temp.dat, dat.prcp, dat.srad))

#--Add watershed and date
out <- cbind.data.frame(st_drop_geometry(aoi.date.poly[,c("HUC12","date.collected")]), out)

#--Write file
write.csv(tmp, paste0(write.path, "temp.srad.by.sample.date.", Sys.Date(), ".csv"), row.names=FALSE)

#---- END ----








#---- PDSI Data ----

year.vec = c(1982,1988,2004, seq(2008,2021,1))

tmp <- extract.terraclim.by.polygons(aoi.value=NULL, aoi.object=aoi.object, by.month=TRUE, year.vec, param.value="palmer", polys, write.file=FALSE)

write.csv(tmp, paste0(write.path, "pdsi.cnty.", Sys.Date(), ".csv"), row.names=FALSE)

#---- END ----





#---- Landcover Data ----

start.year = 2015
end.year = 2020

tmp <- extract.modis.landcover.polygons(aoi.abbr="conus", polys, start.year, end.year, make.proportion=TRUE, write.file=FALSE)

write.csv(tmp, paste0(write.path, "landcover.", Sys.Date(), ".csv"), row.names=FALSE)

#---- END ----



#---- EVI Data ----

year.vec = c(1982,1988,2004, seq(2008,2021,1))

tmp <- extract.modis.evi.polygons(aoi.abbr="conus", polys, year.vec, write.file=FALSE)

write.csv(tmp, paste0(write.path, "evi.cnty.", Sys.Date(), ".csv"), row.names=FALSE)

#---- END ----



#---- Snow Data ----

year.vec = seq(2015,2022,1)

tmp <- extract.modis.snow.polygons(aoi.abbr="conus", polys, year.vec, write.file=FALSE)

write.csv(tmp, paste0(write.path, "snow.", Sys.Date(), ".csv"), row.names=FALSE)

#---- END ----




#---- Generate monthly Precip Totals ----

#--Set polygon summary column
polygon.column.id <- "fips"


#--Read polygon data
sp.path <- "data/Watersheds/"
polys <- st_read(dsn=file.path(sp.path), layer="huc10.watersheds")


#--Get precip data
thredds.path <- "https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/monolevel/"
product.name <- "apcp"
dest.path <- "data/NOAA/"
str.year=1980
end.year=2022


#--Download noaa data
ras <- download.noaa.thredds.data(product.name, thredds.path, dest.path, str.year, end.year, time.out=8*60)
#ras <- load.downloaded.data(dest.path)


#--Extract daily mean by polygons
daily.mean <- extract.by.polygons(ras, aoi.object=polys, poly.id=polygon.column.id, agg.fun='mean', by.polygon=FALSE)


#--Convert to dates
daily.mean[,date:=anytime::anydate(date)]
daily.mean[,year:=lubridate::year(date)]
daily.mean[,month:=lubridate::month(date)]


#--Ensure na values are 0
daily.mean[is.na(value)==TRUE,value:=0]


#--Generate Monthly Totals
time.series <- daily.mean[, lapply(.SD, sum), by=c(polygon.column.id,"year","month"), .SDcols=c("value")]


#--Write file
fst::write_fst(out, paste0(write.path, "prcp.", polygon.column.id, ".", Sys.Date(), ".fst"))

#---- END ----




#---- Generate Standardized Precip Index ----

#--Set huc level
huc.level <- "HUC10"


#--Read polygon data
sp.path <- "C:/DATA/Watersheds/"
polys <- st_read(dsn=file.path(sp.path), layer="huc10.watersheds")


#--Get precip data
thredds.path <- "https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/monolevel/"
product.name <- "apcp"
dest.path <- "C:/DATA/NOAA/"
str.year=1980
end.year=2022


#--Download noaa data
ras <- download.noaa.thredds.data(product.name, thredds.path, dest.path, str.year, end.year, time.out=8*60)
#ras <- load.downloaded.data(dest.path)


#--Extract daily mean by polygons
daily.mean <- extract.by.polygons(ras, aoi.object=polys, poly.id="fips", agg.fun='mean', by.polygon=FALSE)


#--Convert to dates
daily.mean[,date:=anytime::anydate(date)]
daily.mean[,year:=lubridate::year(date)]
daily.mean[,month:=lubridate::month(date)]


#--Ensure na values are 0
daily.mean[is.na(value)==TRUE,value:=0]


#--Generate Monthly Totals
time.series <- daily.mean[, lapply(.SD, sum), by=c(huc.level,"year","month"), .SDcols=c("value")]


#--Loop over polygons
vec <- unlist(unique(time.series[,..huc.level]))

spi.index <- list()

pb <- progress::progress_bar$new(
  format = "  Processing [:bar] :percent eta: :eta",
  total = length(vec), clear = FALSE, width= 60)

for(i in 1:length(vec)){
  pb$tick()

  #--Subset
  t.tmp <- time.series[get(huc.level) %in% vec[i],]

  #--Set Order
  setorder(t.tmp, cols = "year", "month")

  #--Generate SPI
  x <- t.tmp[,value]

  spi.1 <- SPEI::spi(x, scale=1, distribution='Gamma', verbose=FALSE)$fitted
  spi.3 <- SPEI::spi(x, scale=3, distribution='Gamma', verbose=FALSE)$fitted

  #--Store output in list
  tmp <- t.tmp
  tmp[,spi.1:=spi.1]
  tmp[,spi.3:=spi.3]

  spi.index[[i]] <- tmp
}#END



#--Aggregate across year and season
dt.median <- list()

pb <- progress::progress_bar$new(
  format = "  Processing [:bar] :percent eta: :eta",
  total = length(vec), clear = FALSE, width= 60)

for(i in 1:length(spi.index)){
  pb$tick()

  #--Set key
  setkey(spi.index[[i]], month)

  #--Merge index with month lut
  tmp <- merge(spi.index[[i]], lut, by="month",all.x=TRUE)

  #--Aggregate using median
  dt.median[[i]] <- tmp[, lapply(.SD, median), by=c(huc.level,"year","month"), .SDcols=c("spi.1","spi.3")]
}#END loop


#--Merge into single file
out <- rbindlist(dt.median)


#--Write file
fst::write_fst(out, paste0(write.path, "spi.", huc.level, ".", Sys.Date(), ".fst"))


#---- END ----





#---- Temperature by Season ----

huc.level="HUC8"


#--Read Polygon Data
sp.path <- "C:/DATA/Watersheds/"
polys <- st_read(dsn=file.path(sp.path), layer="huc08.watersheds")

#--Set values to download
thredds.path <- "https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/monolevel/"
product.name <- "air.sfc"
dest.path <- "C:/DATA/NOAA/"
str.year = 2005
end.year =2022

#--Download noaa data
#ras <- download.noaa.thredds.data(product.name, thredds.path, dest.path, str.year, end.year, time.out=8*60)
ras <- load.downloaded.data(file.path(dest.path, "air.sfc"))

daily.mean <- extract.by.polygons(ras, polys, poly.id="HUC8", agg.fun='mean', by.polygon=FALSE)
summary(daily.mean)

daily.min <- extract.by.polygons(ras, polys, poly.id="HUC8", agg.fun='min', by.polygon=FALSE)
summary(daily.min)

daily.max <- extract.by.polygons(ras, polys, poly.id="HUC8", agg.fun='max', by.polygon=FALSE)
summary(daily.max)


#--Make list of mean, min, max
out <- list(daily.mean, daily.min, daily.max)
names(out) <- c("mean","min","max")


#--Make season look up table
lut <- cbind.data.frame(month=seq(1,12,1),
                        season=c("winter","winter","winter",
                                 "spring","spring",
                                 "breeding","breeding","breeding",
                                 "fall","fall","fall","fall"))
agg.dat <- list()

#--Convert to dates and make year, month
pb <- progress::progress_bar$new(total = length(out))

for(i in 1:length(out)){
  pb$tick()

  out[[i]] <- out[[i]][,date:=anytime::anydate(date)]
  out[[i]] <- out[[i]][,year:=lubridate::year(date)]
  out[[i]] <- out[[i]][,month:=lubridate::month(date)]

  #--Generate Daily mean, min, max
  if(i==1){
  out[[i]] <- out[[i]][, lapply(.SD, "mean"), by=c(huc.level,"date","year","month"), .SDcols=c("value")]
  }
  if(i==2){
    out[[i]] <- out[[i]][, lapply(.SD, "min"), by=c(huc.level,"date","year","month"), .SDcols=c("value")]
  }
  if(i==3){
    out[[i]] <- out[[i]][, lapply(.SD, "max"), by=c(huc.level,"date","year","month"), .SDcols=c("value")]
  }

  #--Set key
  setkey(out[[i]], month)

  #--Merge index with month lut
  #out[[i]] <- merge(out[[i]], lut, by="month", all.x=TRUE)
}


#--Add temp range
tmp.max <- out[[3]]
tmp.min <- out[[2]]

col.max <- c(huc.level,"date","year","month","value")
col.min <- c("value")

tmp <- cbind(tmp.max[,..col.max], tmp.min[,..col.min])
tmp$range <- abs(tmp[,5] - tmp[,6])

cols <- c("HUC8","date","year","month","range")
tmp <- tmp[,..cols]

colnames(tmp)[which(colnames(tmp)=="range")] <- "value"

out[[4]] <- tmp
names(out) <- c("mean","min","max","range")


#--Generate monthly median by watershed
pb <- progress::progress_bar$new(total = length(out))

for(i in 1:length(out)){
  pb$tick()

  #--Aggregate using median
  agg.dat[[i]] <- out[[i]][, lapply(.SD, median), by=c(huc.level,"year","month"), .SDcols=c("value")]

  #--Change name
  colnames(agg.dat[[i]])[which(colnames(agg.dat[[i]])=="value")] <- paste0("t.", names(out)[i])
}


#--Merge data into single data.table
out <- Reduce(function(x,y){merge(x,y, by=c(huc.level, "year","month"))}, agg.dat)


#--Write file
fst::write_fst(out, paste0(write.path, "temperature.", huc.level, ".", Sys.Date(), ".fst"))

#---- END Temperature by Season ----






#---- MAKE Elevation Data ----

library(elevatr)

sp.path <- "C:/DATA/Cartography.Layers/"
polys <- st_read(dsn=file.path(sp.path), layer="Counties_dtl_geo")



ras <- elevatr::get_elev_raster(polys, z=2)

df <- exactextractr::exact_extract(ras, polys, force_df=TRUE)

for(i in 1:length(df)){
if(i==1){out<-list()}

  x <- df[[i]]$value
  x[x<0] <- 0

out[[i]] <- weighted.mean(x, w=df[[i]]$coverage_fraction)

}
out <- Reduce(rbind, out)
colnames(out) <- "elevation"

out <- cbind(polys, out)
colnames(out) <- tolower(colnames(out))

st_geometry(out) <- NULL

keep.cols <- c("name","state_name","state_fips","cnty_fips","fips","elevation")
out <- out[,keep.cols]

write.csv(out, file.path(write.path, paste0("elevation.", Sys.Date(), ".csv")))
#fst::write_fst(out, paste0(write.path, "elevation.", Sys.Date(), ".fst"))

#---- END ----













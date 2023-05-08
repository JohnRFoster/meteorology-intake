
#-- script for initial download of NOAA data


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

source("R/func/download.noaa.thredds.data.R")
source("R/func/extract.by.polygons.R")

#---- Read Polygon Data ----

polys <- st_read(dsn = "data/counties/", layer="Counties_dtl_geo")
colnames(polys) <- tolower(colnames(polys))

polys <- polys[,c("name","state_name","state_fips","cnty_fips","fips")]

#--------------------------------------------------------
#----------- Get precip data
#--------------------------------------------------------
thredds.path <- "https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/monolevel/"
product.name <- "apcp"
dest.path <- "data/NOAA"
str.year <- 1980
end.year <- lubridate::year(Sys.Date())

#--Set polygon summary column
polygon.column.id <- "fips"


#--Download noaa data
ras <- download.noaa.thredds.data(product.name, thredds.path, dest.path, str.year, end.year, time.out=8*60)

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

#---- Generate Standardized Precip Index ----
#--Set huc level
huc.level <- "HUC10"

#--Generate Monthly Totals -
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


#---- END Precipitation ----




#--------------------------------------------------------
#----------- Get temp data
#--------------------------------------------------------
#---- Temperature by Season ----

huc.level <- "HUC8"

#--Read Polygon Data
sp.path <- "data/Watersheds/"
polys <- st_read(dsn=file.path(sp.path), layer="huc08.watersheds")

#--Set values to download
thredds.path <- "https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/monolevel/"
product.name <- "air.sfc"
dest.path <- "data/NOAA"
str.year = 2005
end.year = lubridate::year(Sys.Date())

#--Download noaa data
ras <- download.noaa.thredds.data(product.name, thredds.path, dest.path, str.year, end.year, time.out=8*60)

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

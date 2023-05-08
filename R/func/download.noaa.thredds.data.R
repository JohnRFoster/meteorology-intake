
#' Download data from NOAA thredds server and return raster brick
#'
#' Generates raster brick of climate products from NOAA thredds server
#'
#' @param product.name base name of file on thredds server
#' @param str.date Start year
#' @param end.date End date
#' @param thredds.path url to data on thredds server (i.e. https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/Dailies/monolevel/)
#'
#' By: Ryan Miller
#'
#' Last updated: 31 Jan 2023
#'

download.noaa.thredds.data <- function(product.name, thredds.path, dest.path, str.year, end.year, time.out){

  #--Load libraries
  require(utils)
  require(raster)
  require(terra)
  require(progress)
  source("R/func/load.downloaded.data.R")

  #Set options
  options(timeout=time.out)
  out.message <- NULL

  #Make download directory
  to.file.path <- file.path(dest.path, product.name)
  dir.create(to.file.path, showWarnings=FALSE)

  # years already downloaded
  years.down <- as.numeric(str_extract(list.files(to.file.path), "\\d{4}"))

  #Make vector of years to download
  years.wanted <- seq(str.year, end.year, 1)
  years <- years.wanted[!years.wanted %in% years.down]

  if(length(years) == 0){
    return(load.downloaded.data(to.file.path))
  } else {

    #Make Storage
    ras <- list()

    print("Getting data...")

    pb <- progress_bar$new(
      format = "  Processing [:bar] :percent eta: :eta",
      total = length(years), clear = FALSE, width= 60)

    #Loop over years to download
    for(i in 1:length(years)){
      pb$tick()

      #Make file name
      file.name <- paste0(product.name,".", years[i],".nc")

      #URL file name
      url.file.name <- file.path(thredds.path, paste0(product.name,".", years[i],".nc"))

      #To file name
      to.file.name <- file.path(to.file.path,file.name)

      #Download file
      if(file.exists(to.file.name)==FALSE){
        utils::download.file(url=url.file.name, destfile=to.file.name, mode="wb", method="libcurl", quiet=FALSE)
      }

      #Read netcdf file using terra::rast into list
      tmp <- try(terra::rast(to.file.name), silent=TRUE)

      if(inherits(tmp, "try-error")==TRUE){
        unlink(to.file.name)
        utils::download.file(url=url.file.name, destfile=to.file.name, mode="wb", method="libcurl", quiet=FALSE)
      }


      tmp <- try(terra::rast(to.file.name), silent=TRUE)

      if(inherits(tmp, "try-error")==TRUE){
        unlink(to.file.name)
        utils::download.file(url=url.file.name, destfile=to.file.name, mode="wb", method="libcurl", quiet=FALSE)
        ras[[i]] <- NA

        out.message <- "Not all rasters downloaded..."
      }

      if(inherits(tmp, "try-error")==FALSE){
        names(tmp) <- as.character(terra::time(tmp))
        ras[[i]] <- tmp
      }


    }#END Loop

    #--Convert list to spat raster stack
    ras <- terra::rast(ras)

    if(is.null(out.message)==FALSE){print(out.message)}

    if(length(years) == length(years.wanted)){
      return(ras)
    } else {
      return(load.downloaded.data(to.file.path))
    }

  }

}#END Function

#---- END END ----

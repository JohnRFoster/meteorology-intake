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

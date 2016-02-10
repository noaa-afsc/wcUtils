#' Retrieve a single zip file from Wildlife Computers Data Portal
#' 
#' \code{wcGetZip} will return a path to a downloaded zip file
#' 
#' The Wildlife Computers Data Portal will return deployment data in the form of a zipped file with 
#' various comma-separated files and other accessory files. 
#' 
#' @param id a single character representing a data portal unique deployment identifier
#'   
#' @return a path to the zip file
#' 
#' @export
wcGetZip <- function(id,keyfile=NULL) {
  download_params <- paste("action=download_deployment&id=",id,sep="")
  r <- wcPOST(keyfile=keyfile,params=download_params)
  temp_file <- tempfile()
  writeBin(httr::content(r, "raw"), temp_file)
  temp_path <- tempfile()
  dir.create(temp_path)
  unzip.fail <- try(unzip(temp_file, exdir=temp_path))
  while(inherits(unzip.fail, "try-error")){
    warning(paste("error unzipping: ",id))
    unlink(temp_file)
    unlink(temp_path)
    temp_file <- tempfile()
    writeBin(httr::content(r, "raw"), temp_file)
    temp_path <- tempfile()
    dir.create(temp_path)
    unzip.fail <- try(unzip(temp_file, exdir=temp_path))
  }
  return(temp_file)
}
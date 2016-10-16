#' Retrieve a single zip file from Wildlife Computers Data Portal
#' 
#' \code{wcGetZip} will return a path to a downloaded zip file
#' 
#' The Wildlife Computers Data Portal will return deployment data in the form of a zipped file with 
#' various comma-separated files and other accessory files. 
#' 
#' @param wc.key public access key (default retrieves from option value set in .Renviron)
#' @param wc.secret secret access key (default retrieves from option value set in .Renviron)
#' @param keyfile path to a json formatted keyfile with wcAccessKey and wcSecretKey

#' @return a path to the zip file
#' 
#' @export
wcGetZip <- function(id,wc.key = Sys.getenv("wcAccessKey"),
                     wc.secret = Sys.getenv("wcSecretKey"),
                     keyfile=NULL) {
  download_params <- paste("action=download_deployment&id=",id,sep = "")
  if (!is.null(keyfile)) {
    keys <- jsonlite::fromJSON(keyfile)
    wc.key <- keys$wcAccessKey
    wc.secret <- keys$wcSecretKey
  }
  
  if (is.null(wc.key) | is.null(wc.secret)) {
    stop("Wildlife Computers keys not found. Either use .Renviron (see help) or a keyfile.json")
  }
  
  x.hash <- digest::hmac(wc.secret,download_params,algo = "sha256")
  r <- httr::POST("http://my.wildlifecomputers.com/services/",
                  body = download_params,
                  httr::add_headers("X-Access" = wc.key,"X-Hash" = x.hash))
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
#' Retrieve data from Wildlife Computers Data Portal
#' 
#' \code{wcGetDownload} will return a list of data frames containing deployment data
#' 
#' The Wildlife Computers Data Portal will return deployment data in the form of a zipped file with 
#' various comma-separated files and other accessory files. The *.csv files correspond to particular
#' data streams. This function, currently, focuses on the locations, behavior, histograms and status
#' data streams.
#' 
#' For most of the files, the data are read in with \code{read.csv} and, other than a few steps to 
#' set the data types, the data are provided 'as is'. The one exception is the histogram data 
#' stream. Here, we use the \code{tidyr} and \code{dplyr} package to 'tidy' the data into a more 
#' appropriate data structure. For now, this is only implemented with timeline data and the 'tidy'd'
#' data is provided within the list element \code{$timelines}
#' 
#' @param id a single character representing a data portal unique deployment identifier
#' @param tidy whether to tidy the histogram data stream and create a timelines output
#'   
#' @return a list of data frames with up to 5 names elemnts (locations, behavior, histograms,
#'   status,timelines)
#' @importFrom magrittr %>%
#' @export
wcGetDownload <- function(id) {
  download_params <- paste("action=download_deployment&id=",id,sep="")
  r <- wcPOST(params=download_params)
  temp_file <- tempfile()
  writeBin(httr::content(r, "raw"), temp_file)
  temp_path <- tempfile()
  dir.create(temp_path)
  unzip.fail <- try(unzip(temp_file, exdir=temp_path))
  while(inherits(unzip.fail, "try-error")){
    cat("error unzipping: ",id,"\n")
    unlink(temp_file)
    unlink(temp_path)
    temp_file <- tempfile()
    writeBin(httr::content(r, "raw"), temp_file)
    temp_path <- tempfile()
    dir.create(temp_path)
    unzip.fail <- try(unzip(temp_file, exdir=temp_path))
  }
  loc_file <- list.files(temp_path,full.names=TRUE,pattern="*-Locations.csv")
  behav_file <- list.files(temp_path,full.names=TRUE,pattern='*-Behavior.csv')
  histo_file <- list.files(temp_path,full.names=TRUE,pattern='*-Histos.csv')
  status_file <- list.files(temp_path,full.names=TRUE,pattern='*-Status.csv')
  df_list <- vector("list")
  if(length(loc_file)==1){
  df_list$locations <- wcUtils::read_locs(loc_file)
  }
  if(length(behav_file)==1) {
  df_list$behavior <- wcUtils::read_behav(behav_file)
  }
  if(length(histo_file)==1) {
  df_list$histos <- wcUtils::read_histos(histo_file)
  }
  if(length(status_file)==1) {
    test <- try(readr::read_csv(status_file),silent = TRUE)
    if(!inherits(test,"try-error")) {
      df_list$status <- readr::read_csv(
        status_file)
    }
    else {
      df_list$status <- readr::read_csv(
        status_file,skip=1
      )
    }
  }

  unlink(temp_path)
  unlink(temp_file)
  return(df_list)
}
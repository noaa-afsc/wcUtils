#' Retrieve data from Wildlife Computers Data Portal
#' 
#' \code{wcGetDownload} will return a list of data frames containing deployment data
#' 
#' The Wildlife Computers Data Portal will return deployment data in the form of a zipped file with 
#' various comma-separated files and other accessory files. The *.csv files correspond to particular
#' data streams. This function, currently, focuses on the locations, behavior, histograms, timelines, status, and messages
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
#' @return a list of data frames with up to 6 named elemnts (locations, behavior, histograms,
#'   status,timelines,messages)
#' @importFrom magrittr %>%
#' @export
wcGetDownload <- function(id,wc.key=Sys.getenv("WCACCESSKEY"),
                          wc.secret=Sys.getenv("WCSECRETKEY"),
                          keyfile=NULL, tidy=TRUE) {
  download_params <- paste("action=download_deployment&id=",id,sep="")
    
    if (!is.null(keyfile)) {
      keys <- jsonlite::fromJSON(keyfile)
      wc.key <- keys$WCACCESSKEY
      wc.secret <- keys$WCSECRETKEY
      r <- wcPOST(wc.key,wc.secret,
                  params=download_params)
    }
  if (!is.null(wc.key) & !is.null(wc.secret)) {
    r <- wcPOST(wc.key,wc.secret,params=download_params)
  }
    
    if (is.null(wc.key) | is.null(wc.secret)) {
      stop("Wildlife Computers keys not found. Please specify in .Renviron (see help) or other environment variable")
    }
  
  temp_file <- tempfile()
  writeBin(httr::content(r, "raw"), temp_file)
  temp_path <- tempfile()
  dir.create(temp_path)
  
  options(warn = 2)
  unzip.fail <- try(unzip(temp_file, exdir=temp_path))
  
  while(inherits(unzip.fail, "try-error")){
    message(paste("error unzipping: ",id))
    unlink(temp_file)
    unlink(temp_path)
    temp_file <- tempfile()
    r <- wcPOST(wc.key,wc.secret,
                params=download_params)
    writeBin(httr::content(r, "raw"), temp_file)
    temp_path <- tempfile()
    dir.create(temp_path)
    unzip.fail <- try(unzip(temp_file, exdir=temp_path))
  }
  
  options(warn = 0)
  
  
  loc_file <- list.files(temp_path,full.names=TRUE,pattern="^\\w+-Locations\\.csv$")
  fastgps_file <- list.files(temp_path,full.names=TRUE,pattern="^\\w+-\\d+-FastGPS\\.csv$")
  all_locs_file <- list.files(temp_path, full.names=TRUE, pattern = "-[0-9]+-Locations.csv")
  behav_file <- list.files(temp_path,full.names=TRUE,pattern="^\\w+-Behavior\\.csv$")
  histo_file <- list.files(temp_path,full.names=TRUE,pattern="^\\w+-Histos\\.csv$")
  ecdf_file <- list.files(temp_path,full.names=TRUE,pattern ="^\\w+-ECDHistos\\.csv$")
  pdt_file <- list.files(temp_path,full.names=TRUE,pattern="^\\w+-PDTs\\.csv$")
  status_file <- list.files(temp_path,full.names=TRUE,pattern="^\\w+-Status\\.csv$")
  messages_file <- list.files(temp_path,full.names=TRUE,pattern="^\\w+-All\\.csv$")
  
  df_list <- vector("list")
  
  if(!rlang::is_empty(loc_file)){
    df_list$locations <- read_locs(loc_file)
  }
  if(!rlang::is_empty(fastgps_file)){
    df_list$fastgps <- read_fastGPS(fastgps_file)
  }
  if(!rlang::is_empty(all_locs_file)){
    df_list$all_locations <- read_locs(all_locs_file)
  }
  if(!rlang::is_empty(behav_file)) {
  df_list$behavior <- read_behav(behav_file)
  }
  if(!rlang::is_empty(ecdf_file)) {
    df_list$ecdf <- read_ecdf(ecdf_file)
  }
  if(!rlang::is_empty(pdt_file)) {
    df_list$pdt <- read_pdt(pdt_file)
  }
  if(!rlang::is_empty(histo_file)) {
  df_list$histos <- read_histos(histo_file)
  if (tidy) { 
    df_list$timelines <- tidyTimelines(df_list$histos)
    }
  }
  if(!rlang::is_empty(messages_file)) {
    df_list$messages <- read_allmsg(messages_file)
  }
  # if(length(status_file)==1) {
  #   test <- try(readr::read_csv(status_file,
  #                               progress = FALSE,
  #                               show_col_types = FALSE),
  #               silent = TRUE)
  #   if(!inherits(test,"try-error")) {
  #     df_list$status <- readr::read_csv(
  #       status_file,
  #       progress = FALSE,
  #       show_col_types = FALSE)
  #   }
  #   else {
  #     df_list$status <- readr::read_csv(
  #       status_file,
  #       progress = FALSE,
  #       show_col_types = FALSE,
  #       skip=1
  #     )
  #   }
  # }
  Sys.sleep(3)
  unlink(temp_path)
  unlink(temp_file)
  return(df_list)
}
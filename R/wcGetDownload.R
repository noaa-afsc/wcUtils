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
#'   status,[timelines])
wcGetDownload <- function(id, tidy=TRUE) {
  download_params <- paste("action=download_deployment&id=",id,sep="")
  r <- wcPOST(params=download_params)
  temp_file <- tempfile()
  writeBin(content(r, "raw"), temp_file)
  temp_path <- tempfile()
  dir.create(temp_path)
  unzip(temp_file, exdir=temp_path)
  loc_file <- list.files(temp_path,full.names=TRUE,pattern="*-Locations.csv")
  behav_file <- list.files(temp_path,full.names=TRUE,pattern='*-Behavior.csv')
  histo_file <- list.files(temp_path,full.names=TRUE,pattern='*-Histos.csv')
  status_file <- list.files(temp_path,full.names=TRUE,pattern='*-Status.csv')
  df_list <- vector("list")
  df_list$locations <- read.csv(loc_file,stringsAsFactors=FALSE)
  df_list$behavior <- read.csv(behav_file,stringsAsFactors=FALSE)
  df_list$histos <- read.csv(histo_file,stringsAsFactors=FALSE)
  df_list$status <- read.csv(status_file,stringsAsFactors=FALSE)
  for(i in c("locations","behavior","histos","status")) {
    names(df_list[[i]]) <- tolower(names(df_list[[i]]))
  }
  for(i in c("locations","behavior","histos","status")) {
    df_list[[i]]$ptt <- factor(df_list[[i]]$ptt,
                               levels=as.character(sort(as.numeric(unique(df_list[[i]]$ptt)))))
    df_list[[i]]$deployid <- factor(df_list[[i]]$deployid,
                               levels=as.character(sort(as.numeric(unique(df_list[[i]]$deployid)))))
  }
  df_list$locations <- df_list$locations %>% 
    dplyr::mutate(date = lubridate::parse_date_time(date,"%H:%M:%S d-b-Y",tz="UTC")) %>%
    data.table::setnames(
      c("date","error.radius",
        "error.semi.major.axis",
        "error.semi.minor.axis",
        "error.ellipse.orientation"),
      c("datadatetime",
        "error_radius",
        "error_semi_major_axis",
        "error_semi_minor_axis",
        "error_ellipse_orientation"))
  if(tidy==TRUE) {
    df_list$timelines <- tidyHistos(df_list$histos)
  }
  return(df_list)
}
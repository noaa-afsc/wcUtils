#' Parse a *-FastGPS.csv files into a proper data.frame
#'
#' @param gps_file file path or file connection to a *-FastGPS.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a data frame
#' @export
read_gps <- function(gps_file,to_lower = TRUE) {
  gps_lines <- readr::read_lines(file = gps_file)
  
  blank_rows <- function(gps_list) {
    row_check <- lapply(gps_list,function(x)
      nchar(x) < 10)
    return(which(row_check == TRUE))
  }
  
  metadata_row <- function(gps_list) {
    row_check <- lapply(gps_list,function(x)
      substring(x,1,1) == ";")
    return(which(row_check == TRUE))
  }
  
  #find the row with colnames and populate gps_colnames vector
  colnames_row <- function(gps_list) {
    row_check <-
      lapply(gps_list[1:10],function(x)
        grepl("Latitude",x))
    return(which(row_check == TRUE))
  }
  
  gps_colnames <-
    unlist(strsplit(gps_lines[[colnames_row(gps_lines)]],','))
  gps_lines <- gps_lines[-c(metadata_row(gps_lines),blank_rows(gps_lines),
                                colnames_row(gps_lines))]
  
  gps_lines <- lapply(gps_lines,function(x)
    strsplit(x,','))
  
  check_line_length <- lapply(gps_lines,
                              function(x) length(gps_colnames) != length(unlist(x))
    )
  check_line_length <- unlist(check_line_length)
  
  gps_lines[check_line_length] <- lapply(
    gps_lines[check_line_length],
    function(x) c(x[[1]], rep.int("",length(gps_colnames)-length(unlist(x[[1]]))))
    )
  
  gps <- do.call("rbind", lapply(gps_lines,
                                    function(x)
                                      t(
                                        data.frame(x,row.names = NULL,stringsAsFactors = FALSE)
                                      )))
  rownames(gps) <- NULL
  
  gps_df <- data.frame(gps,stringsAsFactors = FALSE)
  colnames(gps_df) <- gsub(" ", "_", gps_colnames)
  colnames(gps_df) <- gsub("-","",colnames(gps_df))
  if (to_lower == TRUE) {
    colnames(gps_df) <- tolower(colnames(gps_df))
  }
  
  gps_df <- gps_df[,1:19]
  
  #create our list of desired data types
  
  coltypes_list <- list(
    name = readr::col_character(),
    day = readr::col_character(),
    time = readr::col_character(),
    count = readr::col_numeric(),
    time_offset = readr::col_numeric(),
    locnumber = readr::col_numeric(),
    failures = readr::col_numeric(),
    hauled_out = readr::col_numeric(),
    satellites = readr::col_numeric(),
    initlat = readr::col_numeric(),
    initlon = readr::col_numeric(),
    inittime = readr::col_datetime(format = "%H:%M:%S %d-%b-%Y",tz="UTC"),
    inittype = readr::col_character(),
    latitude = readr::col_numeric(),
    longitude = readr::col_numeric(),
    height = readr::col_numeric(),
    bad_sats = readr::col_character(),
    residual = readr::col_numeric(),
    time_error = readr::col_numeric()
  )
  
  strip_quotes <- function(s) gsub("\"","",s)
  
  gps_df <- readr::type_convert(gps_df,col_types = coltypes_list)  %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(date_time = lubridate::parse_date_time(paste(day,time),"dbYHMS",tz="UTC")) %>% 
    dplyr::rename(deployid=name) %>% 
    dplyr::select(-day,-time) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,date_time) 
  
  return(gps_df)
}
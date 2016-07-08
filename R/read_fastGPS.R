#' Parse a *-FastGPS.csv files into a proper data.frame
#'
#' @param gps_file file path or file connection to a *-FastGPS.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a data frame
#' @export
read_fastGPS <- function(gps_file,to_lower = TRUE) {
  wcUtils:::fixCSV(gps_file,overwrite = TRUE, skip=4)
  gps_df <- data.table::fread(gps_file, 
                              skip=4,check.names=TRUE,
                              data.table=FALSE) 
  
  colnames(gps_df) <- gsub(" ", "_", colnames(gps_df))
  colnames(gps_df) <- gsub("-","",colnames(gps_df))
  if (to_lower == TRUE) {
    colnames(gps_df) <- tolower(colnames(gps_df))
  }
  
  gps_df <- gps_df %>% select(name:cnr)
  
  strip_quotes <- function(s) gsub("\"","",s)
  
  gps_df <- gps_df %>% 
    dplyr::rowwise(.) %>% 
    dplyr::mutate(date_time = lubridate::parse_date_time(paste(day,time),"dbYHMS",tz = "UTC")) %>% 
    dplyr::rename(deployid = name) %>% 
    dplyr::select(-day,-time) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,date_time) %>% 
    data.frame()
  
  return(gps_df)
}
#' Parse a *-Locations.csv files into a proper data.frame
#'
#' @param loc_file file path or file connection to a *-Locations.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a data frame
#' @export
read_locs <- function(loc_file,to_lower = TRUE) {
  wcUtils:::fixCSV(loc_file,overwrite = TRUE)
  loc_df <- data.table::fread(loc_file,
                              data.table=FALSE) 
  
  colnames(loc_df) <- gsub(" ", "_", colnames(loc_df))
  colnames(loc_df) <- gsub("-","",colnames(loc_df))
  if (to_lower == TRUE) {
    colnames(loc_df) <- tolower(colnames(loc_df))
  }
  
  strip_quotes <- function(s) gsub("\"","",s)
  
  loc_df <- loc_df %>% 
    mutate(
      ptt = as.character(ptt),
      date = lubridate::parse_date_time(date,"%H:%M:%S %d-%b-%Y")
    ) %>% 
    dplyr::rowwise(.) %>% 
    dplyr::mutate(deployid = strip_quotes(deployid)) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,date) %>%
    dplyr::rename(date_time = date) %>% 
    data.frame()
  
  return(loc_df)
}

#' Parse a *-Behavior.csv file into a proper data.frame
#'
#' @param behav_file file path or file connection to a *-Behavior.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a data frame
#' @export
read_behav <- function(behav_file,to_lower = TRUE) {
  wcUtils:::fixCSV(behav_file,overwrite = TRUE)
  behav_df <- data.table::fread(behav_file,
                                data.table=FALSE)
  behav_df <- behav_df[,-c(16:27)]
  
  colnames(behav_df) <- gsub(" ", "_", colnames(behav_df))
  colnames(behav_df) <- gsub("-","",colnames(behav_df))
  if (to_lower == TRUE) {
    colnames(behav_df) <- tolower(colnames(behav_df))
  }
  
  strip_quotes <- function(s) gsub("\"","",s)
  
  behav_df <- behav_df %>% 
    mutate(
      ptt = as.character(ptt),
      start = lubridate::parse_date_time(start,"%H:%M:%S %d-%b-%Y"),
      end = lubridate::parse_date_time(end,"%H:%M:%S %d-%b-%Y")
    ) %>% 
    dplyr::rowwise(.) %>% 
    dplyr::mutate(deployid = strip_quotes(deployid)) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,start) %>%
    data.frame()
  
  return(behav_df)
  
}
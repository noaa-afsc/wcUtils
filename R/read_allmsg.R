#' Parse a *-All.csv file into a proper data.frame
#'
#' @param allmsg_file file path or file connection to a *-All.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a data frame
#' @export
read_allmsg <- function(allmsg_file,to_lower = TRUE) {
  wcUtils:::fixCSV(allmsg_file,overwrite = TRUE)
  allmsg_df <- data.table::fread(allmsg_file,
                                data.table = FALSE)
  
  colnames(allmsg_df) <- gsub(" ", "_", colnames(allmsg_df))
  colnames(allmsg_df) <- gsub("-","",colnames(allmsg_df))
  if (to_lower == TRUE) {
    colnames(allmsg_df) <- tolower(colnames(allmsg_df))
  }
  
  strip_quotes <- function(s) gsub("\"","",s)
  
  allmsg_df <- allmsg_df %>% 
    dplyr::mutate(
      platform_id_no. = as.character(platform_id_no.),
      msg_date = lubridate::parse_date_time(msg_date,orders = "mdY HMS")
    ) %>% 
    dplyr::rename(ptt = platform_id_no.) %>% 
    dplyr::rowwise(.) %>% 
    dplyr::mutate(deployid = strip_quotes(deployid)) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,msg_date) %>%
    data.frame()
  
  return(allmsg_df)
  
}
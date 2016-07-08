#' Parse a *-Histos.csv files into a proper data.frame
#'
#' @param histo_file file path or file connection to a *-Histos.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a list of two data frames
#' @export
read_histos <- function(histo_file,to_lower = TRUE) {
  wcUtils:::fixCSV(histo_file,overwrite = TRUE)
  histos_df <- data.table::fread(histo_file,
                                 data.table=FALSE) 
  
  colnames(histos_df) <- sub(" ", "_", colnames(histos_df))
  if (to_lower == TRUE) {
    colnames(histos_df) <- tolower(colnames(histos_df))
  }
  #find the rows with the histo limit values and populate the histo_limits df
  histo_limits <- histos_df %>% 
    filter(grepl("LIMITS",histtype))
  
  if (nrow(histo_limits) > 0) {
    histo_limits <- histo_limits %>% 
  #crazy method for removing all of the columns with only NA values
  .[, apply(
    apply(
      .[1:(ncol(.))], 
      2, 
      is.na),
    2,sum) == 0] %>% 
    select(-depthsensor,-source,-date) %>% 
    data.frame()
  }
  
  strip_quotes <- function(s) gsub("\"","",s)
  
  histos_df <- histos_df %>% 
    dplyr::filter(!grepl("LIMITS",histtype)) %>% 
    dplyr::mutate_each(funs(as.numeric), dplyr::starts_with("bin")) %>% 
    dplyr::mutate(ptt = as.character(ptt),
                  date = lubridate::parse_date_time(date,"%H:%M:%S %d-%b-%Y"),
                  locationquality = as.character(locationquality),
                  latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude)) %>% 
    dplyr::rowwise(.) %>% 
    dplyr::mutate(deployid = strip_quotes(deployid)) %>% 
    data.frame()

  return(list(histos = histos_df,limits = histo_limits))
}

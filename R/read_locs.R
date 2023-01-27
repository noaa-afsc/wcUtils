#' Parse a *-Locations.csv files into a proper data.frame
#'
#' @param loc_file file path or file connection to a *-Locations.csv file
#' @param fix_csv whether to attemtp to fix any comma, csv issues
#'
#' @return a data frame
#' @export
read_locs <- function(loc_file,fix_csv = FALSE) {
  if (fix_csv) {
    fixCSV(loc_file,overwrite = TRUE)
  }
  
  col_types <- readr::cols(
    DeployID = readr::col_character(),
    Ptt = readr::col_character(),
    Instr = readr::col_character(),
    Date = readr::col_datetime("%H:%M:%S %d-%b-%Y"),
    Type = readr::col_character(),
    Quality = readr::col_character(),
    Latitude = readr::col_double(),
    Longitude = readr::col_double(),
    `Error radius` = readr::col_integer(),
    `Error Semi-major axis` = readr::col_integer(),
    `Error Semi-minor axis` = readr::col_integer(),
    `Error Ellipse orientation` = readr::col_integer(),
    Offset = readr::col_character(),
    `Offset orientation` = readr::col_character(),
    `Error Radius` = readr::col_integer(),
    `Error Semi-Major Axis` = readr::col_integer(),
    `Error Semi-Minor Axis` = readr::col_integer(),
    `Error Ellipse Orientation` = readr::col_integer(),
    `Offset Orientation` = readr::col_character(),
    `GPE MSD` = readr::col_character(),
    `GPE U` = readr::col_character(),
    Count = readr::col_integer(),
    Comment = readr::col_character()
  )
  
  loc_df <- readr::read_csv(loc_file, col_types = col_types) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(deployid = deploy_id)
  
  
  loc_df <- loc_df %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,date) %>%
    dplyr::rename(date_time = date) %>% 
    data.frame()
  
  return(loc_df)
}

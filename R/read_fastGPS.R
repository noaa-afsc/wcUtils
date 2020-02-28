#' Parse a *-FastGPS.csv files into a proper data.frame
#'
#' @param gps_file file path or file connection to a *-FastGPS.csv file
#' @param to_lower whether to convert the column names to lower case
#' @param fix_csv whether to attemtp to fix any comma, csv issues
#'
#' @return a data frame
#' @export
read_fastGPS <- function(gps_file,to_lower = TRUE, fix_csv = FALSE) {
  if (fix_csv) {
  wcUtils:::fixCSV(gps_file, overwrite = TRUE, skip = 4)
  }
  
  col_types <- readr::cols_only(
    Name = readr::col_character(),
    Day = readr::col_character(),
    Time = readr::col_character(),
    Count = readr::col_integer(),
    `Time Offset` = readr::col_double(),
    LocNumber = readr::col_integer(),
    Failures = readr::col_integer(),
    `Hauled Out` = readr::col_integer(),
    Satellites = readr::col_integer(),
    InitLat = readr::col_double(),
    InitLon = readr::col_double(),
    InitTime = readr::col_character(),
    InitType = readr::col_character(),
    Latitude = readr::col_double(),
    Longitude = readr::col_double(),
    Height = readr::col_character(),
    `Bad Sats` = readr::col_character(),
    Residual = readr::col_character(),
    `Time Error` = readr::col_character(),
    `TWIC Power` = readr::col_character(),
    `Fastloc Power` = readr::col_character(),
    Noise = readr::col_character(),
    `Range Bits` = readr::col_integer(),
    Id = readr::col_integer(),
    Range = readr::col_integer(),
    Signal = readr::col_character(),
    Doppler = readr::col_character(),
    CNR = readr::col_character()
)
  
  gps_df <- readr::read_csv(gps_file, col_types = col_types,
                            skip = 3) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(deployid = deploy_id)
  
  gps_df <- gps_df %>% 
    dplyr::mutate(date_time = lubridate::parse_date_time(paste(day,time),"dbYHMS",tz = "UTC")) %>% 
    dplyr::rename(deployid = name) %>% 
    dplyr::select(-day,-time) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,date_time) %>% 
    data.frame()
  
  return(gps_df)
}
#' Parse a *-All.csv file into a proper data.frame
#'
#' @param allmsg_file file path or file connection to a *-All.csv file
#' @param to_lower whether to convert the column names to lower case
#' @param fix_csv whether to attemtp to fix any comma, csv issues
#'
#' @return a data frame
#' @export
read_allmsg <- function(allmsg_file,to_lower = TRUE, fix_csv = FALSE) {
  if (fix_csv) {
    wcUtils:::fixCSV(allmsg_file,overwrite = TRUE)
  }
  
  col_types <- readr::cols(
    DeployID = readr::col_character(),
    `Platform ID No.` = readr::col_character(),
    `Prg No.` = readr::col_integer(),
    Latitude = readr::col_double(),
    Longitude = readr::col_double(),
    `Loc. quality` = readr::col_character(),
    `Loc. date` = readr::col_datetime(format = "%m/%d/%Y %H:%M:%S"),
    `Loc. type` = readr::col_character(),
    Altitude = readr::col_integer(),
    Pass = readr::col_integer(),
    Sat. = readr::col_character(),
    `Mote Id` = readr::col_character(),
    Frequency = readr::col_double(),
    `Msg Date` = readr::col_datetime(format = "%m/%d/%Y %H:%M:%S"),
    Comp. = readr::col_integer(),
    Msg = readr::col_integer(),
    `> - 120 DB` = readr::col_integer(),
    `Best level` = readr::col_integer(),
    `Delta freq.` = readr::col_double(),
    `Long. 1` = readr::col_double(),
    `Lat. sol. 1` = readr::col_double(),
    `Long. 2` = readr::col_double(),
    `Lat. sol. 2` = readr::col_double(),
    `Loc. idx` = readr::col_integer(),
    Nopc = readr::col_integer(),
    `Error radius` = readr::col_integer(),
    `Semi-major axis` = readr::col_integer(),
    `Semi-minor axis` = readr::col_integer(),
    `Ellipse orientation` = readr::col_integer(),
    GDOP = readr::col_integer(),
    `SENSOR #01` = readr::col_integer(),
    `SENSOR #02` = readr::col_integer(),
    `SENSOR #03` = readr::col_integer(),
    `SENSOR #04` = readr::col_integer(),
    `SENSOR #05` = readr::col_integer(),
    `SENSOR #06` = readr::col_integer(),
    `SENSOR #07` = readr::col_integer(),
    `SENSOR #08` = readr::col_integer(),
    `SENSOR #09` = readr::col_integer(),
    `SENSOR #10` = readr::col_integer(),
    `SENSOR #11` = readr::col_integer(),
    `SENSOR #12` = readr::col_integer(),
    `SENSOR #13` = readr::col_integer(),
    `SENSOR #14` = readr::col_integer(),
    `SENSOR #15` = readr::col_integer(),
    `SENSOR #16` = readr::col_integer(),
    `SENSOR #17` = readr::col_integer(),
    `SENSOR #18` = readr::col_integer(),
    `SENSOR #19` = readr::col_integer(),
    `SENSOR #20` = readr::col_integer(),
    `SENSOR #21` = readr::col_integer(),
    `SENSOR #22` = readr::col_integer(),
    `SENSOR #23` = readr::col_integer(),
    `SENSOR #24` = readr::col_integer(),
    `SENSOR #25` = readr::col_integer(),
    `SENSOR #26` = readr::col_integer(),
    `SENSOR #27` = readr::col_integer(),
    `SENSOR #28` = readr::col_integer(),
    `SENSOR #29` = readr::col_integer(),
    `SENSOR #30` = readr::col_integer(),
    `SENSOR #31` = readr::col_integer(),
    `SENSOR #32` = readr::col_character()
  )
  
  allmsg_df <- readr::read_csv(allmsg_file, col_types = col_types) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(deployid = deploy_id)
  
  allmsg_df <- allmsg_df %>% 
    dplyr::rename(ptt = platform_id_no) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,msg_date) %>%
    data.frame()
  
  return(allmsg_df)
  
}
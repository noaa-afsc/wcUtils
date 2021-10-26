#' Parse a *-Histos.csv files into a proper data.frame
#'
#' @param histo_file file path or file connection to a *-Histos.csv file
#' @param to_lower whether to convert the column names to lower case
#' @param dt_fmt format for the Date column
#' @param fix_csv whether to attemtp to fix any comma, csv issues
#'
#' @return a list of two data frames
#' @export
read_histos <- function(histo_file, to_lower = TRUE, dt_fmt = "%H:%M:%S %d-%b-%Y", fix_csv = FALSE) {
  if (fix_csv) {
    fixCSV(histo_file,overwrite = TRUE)
  }
  
  col_types <- readr::cols(
    DeployID = readr::col_character(),
    Ptt = readr::col_character(),
    DepthSensor = readr::col_character(),
    Source = readr::col_character(),
    Instr = readr::col_character(),
    HistType = readr::col_character(),
    Date = readr::col_datetime(),
    `Time Offset` = readr::col_double(),
    Count = readr::col_integer(),
    BadTherm = readr::col_integer(),
    LocationQuality = readr::col_character(),
    Latitude = readr::col_double(),
    Longitude = readr::col_double(),
    NumBins = readr::col_integer(),
    Sum = readr::col_double(),
    Bin1 = readr::col_character(),
    Bin2 = readr::col_character(),
    Bin3 = readr::col_character(),
    Bin4 = readr::col_character(),
    Bin5 = readr::col_character(),
    Bin6 = readr::col_character(),
    Bin7 = readr::col_character(),
    Bin8 = readr::col_character(),
    Bin9 = readr::col_character(),
    Bin10 = readr::col_character(),
    Bin11 = readr::col_character(),
    Bin12 = readr::col_character(),
    Bin13 = readr::col_character(),
    Bin14 = readr::col_character(),
    Bin15 = readr::col_character(),
    Bin16 = readr::col_character(),
    Bin17 = readr::col_character(),
    Bin18 = readr::col_character(),
    Bin19 = readr::col_character(),
    Bin20 = readr::col_character(),
    Bin21 = readr::col_character(),
    Bin22 = readr::col_character(),
    Bin23 = readr::col_character(),
    Bin24 = readr::col_character(),
    Bin25 = readr::col_character(),
    Bin26 = readr::col_character(),
    Bin27 = readr::col_character(),
    Bin28 = readr::col_character(),
    Bin29 = readr::col_character(),
    Bin30 = readr::col_character(),
    Bin31 = readr::col_character(),
    Bin32 = readr::col_character(),
    Bin33 = readr::col_character(),
    Bin34 = readr::col_character(),
    Bin35 = readr::col_character(),
    Bin36 = readr::col_character(),
    Bin37 = readr::col_character(),
    Bin38 = readr::col_character(),
    Bin39 = readr::col_character(),
    Bin40 = readr::col_character(),
    Bin41 = readr::col_character(),
    Bin42 = readr::col_character(),
    Bin43 = readr::col_character(),
    Bin44 = readr::col_character(),
    Bin45 = readr::col_character(),
    Bin46 = readr::col_character(),
    Bin47 = readr::col_character(),
    Bin48 = readr::col_character(),
    Bin49 = readr::col_character(),
    Bin50 = readr::col_character(),
    Bin51 = readr::col_character(),
    Bin52 = readr::col_character(),
    Bin53 = readr::col_character(),
    Bin54 = readr::col_character(),
    Bin55 = readr::col_character(),
    Bin56 = readr::col_character(),
    Bin57 = readr::col_character(),
    Bin58 = readr::col_character(),
    Bin59 = readr::col_character(),
    Bin60 = readr::col_character(),
    Bin61 = readr::col_character(),
    Bin62 = readr::col_character(),
    Bin63 = readr::col_character(),
    Bin64 = readr::col_character(),
    Bin65 = readr::col_character(),
    Bin66 = readr::col_character(),
    Bin67 = readr::col_character(),
    Bin68 = readr::col_character(),
    Bin69 = readr::col_character(),
    Bin70 = readr::col_character(),
    Bin71 = readr::col_character(),
    Bin72 = readr::col_character()
  )
  
  histos_df <- readr::read_csv(histo_file, col_types = col_types) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(deployid = deploy_id)
  
  #find the rows with the histo limit values and populate the histo_limits df
  histo_limits <- histos_df %>% 
    dplyr::filter(grepl("LIMITS",.$hist_type))
  
  if (nrow(histo_limits) > 0) {
    histo_limits <- histo_limits %>% 
      .[,colSums(is.na(.)) < nrow(.)]
  }
  
  histos_df <- histos_df %>% 
    dplyr::filter(!grepl("LIMITS",.$hist_type)) %>% 
    # should update this to dplyr::across() once 1.0 is released
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("bin")), as.numeric) %>% 
    data.frame()

  return(list(histos = histos_df,limits = histo_limits))
}

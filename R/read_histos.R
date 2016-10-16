#' Parse a *-Histos.csv files into a proper data.frame
#'
#' @param histo_file file path or file connection to a *-Histos.csv file
#' @param to_lower whether to convert the column names to lower case
#' @param fix_csv whether to attemtp to fix any comma, csv issues
#'
#' @return a list of two data frames
#' @export
read_histos <- function(histo_file,to_lower = TRUE, fix_csv = FALSE) {
  if (fix_csv) {
    wcUtils:::fixCSV(histo_file,overwrite = TRUE)
  }
  
  col_types <- readr::cols(
    DeployID = readr::col_character(),
    Ptt = readr::col_character(),
    DepthSensor = readr::col_character(),
    Source = readr::col_character(),
    Instr = readr::col_character(),
    HistType = readr::col_character(),
    Date = readr::col_datetime("%H:%M:%S %d-%b-%Y"),
    `Time Offset` = readr::col_double(),
    Count = readr::col_integer(),
    BadTherm = readr::col_integer(),
    LocationQuality = readr::col_character(),
    Latitude = readr::col_double(),
    Longitude = readr::col_double(),
    NumBins = readr::col_integer(),
    Sum = readr::col_integer(),
    Bin1 = readr::col_double(),
    Bin2 = readr::col_double(),
    Bin3 = readr::col_double(),
    Bin4 = readr::col_double(),
    Bin5 = readr::col_double(),
    Bin6 = readr::col_double(),
    Bin7 = readr::col_double(),
    Bin8 = readr::col_double(),
    Bin9 = readr::col_double(),
    Bin10 = readr::col_double(),
    Bin11 = readr::col_double(),
    Bin12 = readr::col_double(),
    Bin13 = readr::col_double(),
    Bin14 = readr::col_double(),
    Bin15 = readr::col_double(),
    Bin16 = readr::col_double(),
    Bin17 = readr::col_double(),
    Bin18 = readr::col_double(),
    Bin19 = readr::col_double(),
    Bin20 = readr::col_double(),
    Bin21 = readr::col_double(),
    Bin22 = readr::col_double(),
    Bin23 = readr::col_double(),
    Bin24 = readr::col_double(),
    Bin25 = readr::col_double(),
    Bin26 = readr::col_double(),
    Bin27 = readr::col_double(),
    Bin28 = readr::col_double(),
    Bin29 = readr::col_double(),
    Bin30 = readr::col_double(),
    Bin31 = readr::col_double(),
    Bin32 = readr::col_double(),
    Bin33 = readr::col_double(),
    Bin34 = readr::col_double(),
    Bin35 = readr::col_double(),
    Bin36 = readr::col_double(),
    Bin37 = readr::col_double(),
    Bin38 = readr::col_double(),
    Bin39 = readr::col_double(),
    Bin40 = readr::col_double(),
    Bin41 = readr::col_double(),
    Bin42 = readr::col_double(),
    Bin43 = readr::col_double(),
    Bin44 = readr::col_double(),
    Bin45 = readr::col_double(),
    Bin46 = readr::col_double(),
    Bin47 = readr::col_double(),
    Bin48 = readr::col_double(),
    Bin49 = readr::col_double(),
    Bin50 = readr::col_double(),
    Bin51 = readr::col_double(),
    Bin52 = readr::col_double(),
    Bin53 = readr::col_double(),
    Bin54 = readr::col_double(),
    Bin55 = readr::col_double(),
    Bin56 = readr::col_double(),
    Bin57 = readr::col_double(),
    Bin58 = readr::col_double(),
    Bin59 = readr::col_double(),
    Bin60 = readr::col_double(),
    Bin61 = readr::col_double(),
    Bin62 = readr::col_double(),
    Bin63 = readr::col_double(),
    Bin64 = readr::col_double(),
    Bin65 = readr::col_double(),
    Bin66 = readr::col_double(),
    Bin67 = readr::col_double(),
    Bin68 = readr::col_double(),
    Bin69 = readr::col_double(),
    Bin70 = readr::col_double(),
    Bin71 = readr::col_double(),
    Bin72 = readr::col_double()
  )
  
  histos_df <- readr::read_csv(histo_file, col_types = col_types) 
  
  colnames(histos_df) <- sub(" ", "_", colnames(histos_df))
  if (to_lower == TRUE) {
    colnames(histos_df) <- tolower(colnames(histos_df))
  }
  #find the rows with the histo limit values and populate the histo_limits df
  histo_limits <- histos_df %>% 
    filter(grepl("LIMITS",histtype))
  
  if (nrow(histo_limits) > 0) {
    histo_limits <- histo_limits %>% 
      .[,colSums(is.na(.))<nrow(.)]
  }
  strip_quotes <- function(s) gsub("\"","",s)
  
  histos_df <- histos_df %>% 
    dplyr::filter(!grepl("LIMITS",histtype)) %>% 
    dplyr::mutate(deployid = strip_quotes(deployid)) %>% 
    data.frame()

  return(list(histos = histos_df,limits = histo_limits))
}

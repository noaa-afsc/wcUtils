#' Parse a *-Locations.csv files into a proper data.frame
#'
#' @param loc_file file path or file connection to a *-Histos.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a data frame
#' @export
read_locs <- function(loc_file,to_lower = TRUE) {
  loc_lines <- readr::read_lines(file = loc_file)
  
  blank_rows <- function(loc_list) {
    row_check <- lapply(loc_list,function(x)
      nchar(x) < 10)
    return(which(row_check == TRUE))
  }
  
  #find the row with colnames and populate loc_colnames vector
  colnames_row <- function(loc_list) {
    row_check <-
      lapply(loc_list[1:10],function(x)
        grepl("Latitude",x))
    return(which(row_check == TRUE))
  }
  
  loc_colnames <-
    unlist(strsplit(loc_lines[[colnames_row(loc_lines)]],','))
  loc_lines <- loc_lines[-c(blank_rows(loc_lines),
                                colnames_row(loc_lines))]
  
  loc_lines <- lapply(loc_lines,function(x)
    strsplit(x,','))
  
  check_line_length <- lapply(loc_lines,
                              function(x) length(loc_colnames) != length(unlist(x))
    )
  check_line_length <- unlist(check_line_length)
  
  loc_lines[check_line_length] <- lapply(
    loc_lines[check_line_length],
    function(x) c(x[[1]], rep.int("",length(loc_colnames)-length(unlist(x[[1]]))))
    )
  
  locs <- do.call("rbind", lapply(loc_lines,
                                    function(x)
                                      t(
                                        data.frame(x,row.names = NULL,stringsAsFactors = FALSE)
                                      )))
  rownames(locs) <- NULL
  
  locs_df <- data.frame(locs,stringsAsFactors = FALSE)
  colnames(locs_df) <- gsub(" ", "_", loc_colnames)
  colnames(locs_df) <- gsub("-","",colnames(locs_df))
  if (to_lower == TRUE) {
    colnames(locs_df) <- tolower(colnames(locs_df))
  }
  
  #create our list of desired data types
  
  coltypes_list <- list(
    deployid = readr::col_character(),
    ptt = readr::col_character(),
    instr = readr::col_character(),
    date = readr::col_datetime(format = "%H:%M:%S %d-%b-%Y"),
    type = readr::col_character(),
    quality = readr::col_character(),
    latitude = readr::col_double(),
    longitude = readr::col_double(),
    error_radius = readr::col_number(),
    error_semimajor_axis = readr::col_number(),
    error_semiminor_axis = readr::col_number(),
    error_ellipse_orientation = readr::col_number(),
    offset = readr::col_number(),
    offset_orientation = readr::col_number(),
    gpe_msd = readr::col_number(),
    gpe_u = readr::col_number(),
    comment = readr::col_character()
  )
  
  strip_quotes <- function(s) gsub("\"","",s)
  
  locs_df <- readr::type_convert(locs_df,col_types = coltypes_list)  %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(deployid = strip_quotes(deployid)) %>% 
    dplyr::group_by(deployid) %>% 
    dplyr::arrange(deployid,date) %>%
    dplyr::rename(date_time=date) %>% 
    tbl_df()

  
  return(locs_df)
}

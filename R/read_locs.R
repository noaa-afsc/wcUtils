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
    deployid = col_character(),
    ptt = col_character(),
    instr = col_character(),
    date = col_datetime(format = "%H:%M:%S %d-%b-%Y",tz = "UTC"),
    type = col_character(),
    quality = col_character(),
    latitude = col_numeric(),
    longitude = col_numeric(),
    error_radius = col_numeric(),
    error_semimajor_axis = col_numeric(),
    error_semiminor_axis = col_numeric(),
    error_ellipse_orientation = col_numeric(),
    offset = col_numeric(),
    offset_orientation = col_numeric(),
    gpe_msd = col_numeric(),
    gpe_u = col_numeric(),
    comment = col_character()
  )
  locs_df <- readr::type_convert(locs_df,col_types = coltypes_list)
  return(locs_df)
}
#' Parse a *-Behavior.csv file into a proper data.frame
#'
#' @param behav_file file path or file connection to a *-Histos.csv file
#' @param to_lower whether to convert the column names to lower case
#'
#' @return a data frame
#' @export
read_behav <- function(behav_file,to_lower = TRUE) {
  behav_lines <- readr::read_lines(file = behav_file)
  
  blank_rows <- function( behav_list) {
    row_check <- lapply(behav_list,function(x)
      nchar(x) < 10)
    return(which(row_check == TRUE))
  }
  
  #find the row with colnames and populate loc_colnames vector
  colnames_row <- function(behav_list) {
    row_check <-
      lapply(behav_list[1:10],function(x)
        grepl("DepthSensor",x))
    return(which(row_check == TRUE))
  }
  
  behav_colnames <-
    unlist(strsplit(behav_lines[[colnames_row(behav_lines)]],','))
  behav_lines <- behav_lines[-c(blank_rows(behav_lines),
                           colnames_row(behav_lines))]
  
  behav_lines <- lapply(behav_lines,function(x)
    strsplit(x,','))
  
  check_line_length <- lapply(behav_lines,
                              function(x) length(behav_colnames) != length(unlist(x))
  )
  check_line_length <- unlist(check_line_length)
  
  behav_lines[check_line_length] <- lapply(
    behav_lines[check_line_length],
    function(x) c(x[[1]], rep.int("",length(behav_colnames)-length(unlist(x[[1]]))))
  )
   
  behav <- do.call("rbind", lapply(behav_lines,
                                  function(x)
                                    t(
                                      data.frame(x,row.names = NULL,stringsAsFactors = FALSE)
                                    )))
  
  rownames(behav) <- NULL
  
  behav_df <- data.frame(behav,stringsAsFactors = FALSE)
  colnames(behav_df) <- gsub(" ", "_", behav_colnames)
  colnames(behav_df) <- gsub("-","",colnames(behav_df))
  if (to_lower == TRUE) {
    colnames(behav_df) <- tolower(colnames(behav_df))
  }
  
  behav_df <- behav_df[,-c(16:27)]
  
  #create our list of desired data types
  
  coltypes_list <- list(
    deployid = readr::col_character(),
    ptt = readr::col_character(),
    depthsensor = readr::col_character(),
    source = readr::col_character(),
    instr = readr::col_character(),
    count = readr::col_integer(),
    start = readr::col_datetime(format = "%H:%M:%S %d-%b-%Y"),
    end = readr::col_datetime(format = "%H:%M:%S %d-%b-%Y"),
    what = readr::col_character(),
    number = readr::col_integer(),
    shape = readr::col_character(),
    depthmin = readr::col_number(),
    depthmax = readr::col_number(),
    durationmin = readr::col_number(),
    durationmax = readr::col_number(),
    shallow = readr::col_integer(),
    deep = readr::col_integer()
  )
  behav_df <- readr::type_convert(behav_df,col_types = coltypes_list)
  return(behav_df)
  
}
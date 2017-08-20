#' Expanded make.names function for creating consistent column names
#'
#' @param x data frame with columns to be renamed
#'
#' @return a data frame
#' @export
make_names <- function(x) {
  new_names <- make.names(colnames(x))
  new_names <- gsub("\\.", "_", new_names)
  new_names <- tolower(new_names)
  colnames(x) <- new_names
  x
}
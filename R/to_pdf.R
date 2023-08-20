#' Transform a `wcECDF` Object Into a Probability Density Function
#'
#' @param ecdf two-column data.frame with `ecdf_prop` and `depth_break`
#'
#' @return a two-column data.frame with `ecdf_prop` and `depth_break`
#' @export
#'
to_pdf <- function(ecdf) {
  if(is.null(ecdf)) {
    return(NULL)
  }
  # ecdf <- drop_ecd_duplicates(ecdf)
  ecdf$pdf  <- c(0, diff(ecdf$ecd_prop) / diff(ecdf$depth_break))
  ecdf$propTAD <- c(0, diff(ecdf$ecd_prop))
  return(ecdf)
}

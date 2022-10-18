#' Combine Two `wcECDF` Objects Into a Single ECDF
#'
#' @param ecdf1 object of class `wcECDF`; typically the *shallow* region
#' @param ecdf2 object of class `wcECDF`; typically the *deep* region
#'
#' @return four-column data.frame with columns `ecd_prop`, `pdf`, `prob`, and `depth_break`
#' @export
#'
combine_ecdf <- function(ecdf1, ecdf2, return = 'pdf') {
  if (is.null(ecdf1) & is.null(ecdf2)) {
    return(NULL)
  }
  if (ecdf1$percent_time == 0) {
    return(NULL)
  }
  if (is.null(ecdf2)) {
    if(return == 'pdf') {
      return(to_pdf(ecdf1$ecdf))
    }
    if(return == 'ecdf') {
      ecdf1 <- drop_ecd_duplicates(ecdf1$ecdf)
      return(ecdf1)
    }
  }
  if (!inherits(ecdf1, "wcECDF")) {
    stop("ecdf1 must be of class 'wcECDF'")
  }
  if (!inherits(ecdf2, "wcECDF")) {
    stop("ecdf2 must be of class 'wcECDF'")
  }
  
  dat1 <- ecdf1$ecdf
  w1 <- ecdf1$percent_time / (ecdf1$percent_time + ecdf2$percent_time)
  w2 <- ecdf2$percent_time / (ecdf1$percent_time + ecdf2$percent_time)
  
  dat1$ecd_prop <- dat1$ecd_prop * w1
  
  dat2 <- ecdf2$ecdf
  dat2$ecd_prop <- dat2$ecd_prop * w2
  
  dat2$ecd_prop <- dat2$ecd_prop + dat1$ecd_prop[nrow(dat1)]
  
  if(return == 'pdf') {
    dat <- to_pdf(rbind(dat1, dat2))
  }
  if(return == 'ecdf') {
    dat <- rbind(dat1, dat2)
  }
  return(dat)
}

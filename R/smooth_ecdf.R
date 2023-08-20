#' Create smoothed ECDF
#'
#' @param ecdf
#' @param bin.width
#'
#' @return
#' @export
#'
smooth_ecdf <- function(ecdf, bin.width) {
  if (is.null(ecdf)) {
    return(NULL)
  }
  
  ecdf <- dplyr::arrange(ecdf, depth_break)
  n <- nrow(ecdf)
  max.depth <- max(ecdf$depth_break)
  
  if(max.depth < bin.width) {
    ecdf[nrow(ecdf), "depth_break"] <- bin.width
    max.depth <- bin.width
  }
  
  if(n == 2) {
    fit <- lm(ecd_prop ~ 0 + depth_break,
              data = ecdf, weights = c(1,10))
  }
  if(n == 3) {
    fit <- lm(ecd_prop ~ 0 + depth_break,
              data = ecdf, weights = c(1,1,10))
  }
  if(n > 3) {
    if(n >= 7) {
      k = n-1
    } else {
      k = n
    }
    fit <- scam::scam(ecd_prop ~ s(depth_break, bs = "mpi",k = k),
                      data = ecdf, optimizer ="efs",
                      weights = c(1*n, rep(1,n-2), 10*n))
  }
  smooth_ecdf <- predict(fit, newdata = data.frame(depth_break = seq.int(0,max.depth, bin.width)))
  dat <- data.frame(depth_break = seq.int(0,max.depth,bin.width),ecd_prop = smooth_ecdf)
  if (dat[nrow(dat), "ecd_prop"] > 1.0) {
    floor(dat[nrow(dat), "ecd_prop"])
  }
  return(dat)
  
}

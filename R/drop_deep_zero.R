#' Drop the 0 record from deep ecdf
#'
#' @param e 
#'
#' @return
#' @export
#'
#' @examples
drop_deep_zero <- function(e) {
  if(nrow(e[e$depth_break == 0,]) == 1) {
    return(e)
  }
  e <- e[-which(e$depth_break == 0 & e$ecd_prop > 0),]
  return(e)
}
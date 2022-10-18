#' Drop duplicates within an ECDF
#'
#' @param e
#'
#' @return
#' @export
#'
#' @examples
drop_ecd_duplicates <- function(e) {
  d <- e$depth_break
  if(sum(duplicated(d)) < 1) {
    return(e)
  }
  dup_idx <- unique(
    c(which(duplicated(d)), which(duplicated(d, fromLast = TRUE)))
  )
  dup_idx <- head(sort(dup_idx), -1)
  
  e <- e[-dup_idx,]
}

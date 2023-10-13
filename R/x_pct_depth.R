#' Determine percent TAD at specific depth
#'
#' @param ecdf 
#' @param pct_tad 
#'
#' @return
#' @export
#'
#' @examples
x_pct_depth <- function (ecdf,pct_tad) {
  if (is.null(ecdf)) {
    return(NA)
  }
  t <- spline_ecdf(ecdf = ecdf, bin.width = 1) |> 
    mutate(d = abs(pct_tad - ecd_prop))
  res <- t[which.min(t$d),"depth_break",][[1]]
  return(res)
}


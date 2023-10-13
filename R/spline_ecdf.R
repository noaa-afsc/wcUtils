spline_ecdf <- function(ecdf, bin.width) {
  if (is.null(ecdf)) {
    return(NULL)
  }
  
  ecdf <- ecdf %>% drop_deep_zero() %>% 
    dplyr::arrange(depth_break)
  
  n <- nrow(ecdf)
  max.depth <- max(ecdf$depth_break)
  
  if(max.depth < bin.width) {
    ecdf[nrow(ecdf), "depth_break"] <- bin.width
    max.depth <- bin.width
  }
  
  depth_out <- c(ecdf$depth_break, 
                 seq.int(0,max.depth,bin.width)) |> 
    sort()
  
  depth_out <- depth_out[!duplicated(depth_out)]
  
  spline_ecdf <- spline(ecdf$depth_break, ecdf$ecd_prop, method = "hyman",
                        xout = depth_out)
  
  dat <- tibble(
    depth_break = spline_ecdf$x,
    ecd_prop = spline_ecdf$y
  ) |> 
    dplyr::filter(depth_break %in% seq.int(0,max.depth,bin.width)) |> 
    dplyr::arrange(depth_break)
  
  return(dat)
}

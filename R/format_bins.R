#' format bin labels
#' 
#' @param bins a vector of bin labels to be formated
#' 
#' @return a vector of formated bin labels
format_bins <- function(bins) {
  plus_bins <- which(grepl(">",bins))
  bins[-plus_bins] <- as.integer(bins[-plus_bins])
  bins[plus_bins] <- paste0(">",
                            as.integer(
                            unlist(strsplit(bins[plus_bins],">")[[1]][2])
                            )
                            )
  return(bins)
}
#' Apply 'tidy' data principles to the dive-depth histogram data stream
#' 
#' \code{tidyDiveDepths} returns a 'tidy'd' data frame of dive depth data
#' 
#' The histogram data stream is provided in a 'wide' format (each row represents a time period and the observed values are provided in 1 to 72 'bin' columns). This format can be difficult to work with in R and other data analysis platforms (e.g. database tables), so we use the \code{tidyr} and \code{dplyr} packages to manipulate the data into a more flexible, 'narrow' format. This results in a data structure where every row represents a single observation.
#' 
#' This is implemented, here, with the dive depth data. For dive depth data, the tag records the maximum depth experienced during a qualifying dive and tallies those dives into user-sepcified depth bins and user-specified time bins. This, unlike with timeline data, requires some knowledge of these user-specified bins. As long as the user has uploaded a configuration/report file to the Wildlife Computers Data Portal, then the *-Histos.csv file provides information on the dive depth bins. If the bin information is not available, the function will produce a warning and output files with generic 'Bin' labels.
#' 
#' @param histos an un-tidy'd histogram data frame
#'
#' @return a data frame with tidy, narrow data structure and actual dive depths bin limits (when provided)
#' @export
tidyDiveDepths <- function(histos) {
  types <- dplyr::group_by(histos,histtype)
  t <- dplyr::summarise(types, n = n())
  if (all(t != c('DiveDepthLIMITS'))) {
    warning('No DiveDepthLIMITs found',call. = FALSE)
  }
  if (all(t != c('DiveDepth'))) {
    stop('No DiveDepth data found',call. = FALSE)
  }
  histos <- dplyr::filter(histos,
                          histtype=='DiveDepth')
  divedepth <- histos %>%
    tidyr::gather(bin,num_dives, starts_with('bin')) %>%
    dplyr::rename(datadatetime=date) %>% 
    dplyr::select(one_of(c("deployid","datadatetime","bin","num_dives"))) %>%
    dplyr::arrange(deployid,datadatetime,bin)
}
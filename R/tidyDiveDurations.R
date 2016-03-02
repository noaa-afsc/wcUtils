#' Apply 'tidy' data principles to the dive-duration histogram data stream
#' 
#' \code{tidyDiveDurations} returns a 'tidy'd' data frame of dive duration data
#' 
#' The histogram data stream is provided in a 'wide' format (each row represents a time period and the observed values are provided in 1 to 72 'bin' columns). This format can be difficult to work with in R and other data analysis platforms (e.g. database tables), so we use the \code{tidyr} and \code{dplyr} packages to manipulate the data into a more flexible, 'narrow' format. This results in a data structure where every row represents a single observation.
#' 
#' This is implemented, here, with the dive duration data. For dive uration data, the tag records the duration in seconds of a qualifying dive and tallies those durationso user-sepcified duration bins and user-specified time bins. This, unlike with timeline data, requires some knowledge of these user-specified bins. As long as the user has uploaded a configuration/report file to the Wildlife Computers Data Portal, then the *-Histos.csv file provides information on the dive durations. If the bin information is not available, the function will produce a warning and output files with generic 'Bin' labels.
#' 
#' @param histos a list returned from \code{read_histos}
#'
#' @return a data frame with tidy, narrow data structure and actual dive duration bin limits (when provided)
#' @export
tidyDiveDurations <- function(histos) {
  if(!is.null(histos$limits)) {
  limits <- dplyr::filter(histos$limits,histtype=='DiveDurationLIMITS') %>% 
    dplyr::select(-histtype) %>% 
    tidyr::gather(bin,bin_duration_limit,starts_with('bin'))
  if(nrow(limits)<1) {
    warning("Dive duration limits not correctly specified. Will use generic bin labels")
  }
  }
  histos <- histos$histos
  types <- dplyr::group_by(histos,histtype)
  t <- dplyr::summarise(types, n = n())
  if (all(t$histtype != c('DiveDuration'))) {
    warning('No DiveDuration data found',call. = FALSE)
    return(NULL)
  }
  if(is.null(histos$limits)) {
    warning("No dive duration limits found. Will use generic bin labels",call.=FALSE)
  }
  
  histos <- dplyr::filter(histos,
                          histtype=='DiveDuration')
  if(!is.null(histos$limits) && nrow(limits)==1) {
  diveduration <- histos %>%
    tidyr::gather(bin,num_dives, starts_with('bin')) %>%
    dplyr::rename(datadatetime=date) %>% 
    dplyr::select(one_of(c("deployid","datadatetime","bin","num_dives"))) %>%
    dplyr::inner_join(limits) %>% 
    dplyr::select(deployid,datadatetime,num_dives,bin_duration_limit,bin) %>% 
    dplyr::mutate(bin_duration_limit=format_bins(bin_duration_limit)) %>% 
    dplyr::arrange(deployid,datadatetime,bin) 
  } else {
    diveduration <- histos %>%
      tidyr::gather(bin,num_dives, starts_with('bin')) %>%
      dplyr::rename(datadatetime=date) %>% 
      dplyr::select(one_of(c("deployid","datadatetime","bin","num_dives"))) %>%
      dplyr::select(deployid,datadatetime,num_dives,bin) %>% 
      dplyr::arrange(deployid,datadatetime,bin) 
  }
}
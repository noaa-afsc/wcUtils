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
    limits <- dplyr::filter(histos$limits,hist_type == 'DiveDurationLIMITS') %>% 
      dplyr::select(-hist_type) %>% 
      tidyr::gather(bin,bin_upper_limit,dplyr::starts_with('bin'))
    if(nrow(limits)<1) {
      warning("Dive Duration limits not correctly specified. Will use generic bin labels")
    }
  }
  types <- dplyr::group_by(histos$histos,hist_type)
  t <- dplyr::summarise(types, n = dplyr::n())
  if (all(t$hist_type != c('DiveDuration'))) {
    warning('No Dive Duration data found',call. = FALSE)
    return(NULL)
  }
  if(is.null(histos$limits)) {
    warning("No Dive Duration limits found. Will use generic bin labels",call.=FALSE)
  }
  
  histos <- dplyr::filter(histos$histos,
                          hist_type == 'DiveDuration')
  
  if(!is.null(limits)) {
    diveduration_out <- histos %>%
      tidyr::gather(bin,num_dives, starts_with('bin')) %>%
      dplyr::rename(diveduration_dt=date) %>% 
      dplyr::select(one_of(c("deployid","diveduration_dt","bin","num_dives"))) %>%
      dplyr::inner_join(limits) %>% 
      dplyr::select(deployid,diveduration_dt,bin,bin_upper_limit,num_dives) %>% 
      dplyr::mutate(bin = factor(bin, 
                                 levels = paste0("bin",1:72), 
                                 ordered = TRUE)) %>% 
      dplyr::arrange(deployid,diveduration_dt,bin) 
  } else {
    diveduration_out <- histos %>%
      tidyr::gather(bin,num_dives, starts_with('bin')) %>%
      dplyr::rename(diveduration_dt=date) %>% 
      dplyr::select(one_of(c("deployid","diveduration_dt","bin","num_dives"))) %>%
      dplyr::select(deployid,diveduration_dt,bin,num_dives) %>% 
      dplyr::mutate(bin = factor(bin, 
                                 levels = paste0("bin",1:72), 
                                 ordered = TRUE)) %>% 
      dplyr::arrange(deployid,diveduration_dt,bin) 
  }
  
  return(diveduration_out)
}
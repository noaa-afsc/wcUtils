#' Apply 'tidy' data principles to the time-at-depth histogram data stream
#'
#' \code{tidyTimeAtDepth} returns a 'tidy'd' data frame of time-at-depth data
#'
#' The histogram data stream is provided in a 'wide' format (each row represents
#' a time period and the observed values are provided in 1 to 72 'bin' columns).
#' This format can be difficult to work with in R and other data analysis
#' platforms (e.g. database tables), so we use the \code{tidyr} and \code{dplyr}
#' packages to manipulate the data into a more flexible, 'narrow' format. This
#' results in a data structure where every row represents a single observation.
#'
#' This is implemented, here, with the time-at-depth data. For time-at-depth
#' data, the tag records the portion of time the tag spent within user-defined
#' dive depth bins. This, unlike with timeline data, requires some knowledge of
#' these user-specified bins. As long as the user has uploaded a
#' configuration/report file to the Wildlife Computers Data Portal, then the
#' *-Histos.csv file provides information on the time-at-depth bins. If the bin
#' information is not available, the function will produce a warning and output
#' files with generic 'Bin' labels.
#'
#' @param histos a list returned from \code{read_histos}
#'
#' @return a data frame with tidy, narrow data structure and actual
#'   time-at-depth bin limits (when provided)
#' @export
tidyTimeAtDepth <- function(histos) {
  if(!is.null(histos$limits)) {
  limits <- dplyr::filter(histos$limits,hist_type == 'TADLIMITS') %>% 
    dplyr::select(-hist_type) %>% 
    tidyr::gather(bin,bin_upper_limit,dplyr::starts_with('bin'))
  if(nrow(limits)<1) {
    warning("Time-At-Depth limits not correctly specified. Will use generic bin labels")
  }
  }
  types <- dplyr::group_by(histos$histos,hist_type)
  t <- dplyr::summarise(types, n = dplyr::n())
  if (all(t$hist_type != c('TAD'))) {
    warning('No Time-At-Depth data found',call. = FALSE)
    return(NULL)
  }
  if(is.null(histos$limits)) {
    warning("No TAD limits found. Will use generic bin labels",call.=FALSE)
  }
  
  histos <- dplyr::filter(histos$histos,
                          hist_type == 'TAD')
  if(!is.null(limits)) {
  tad_out <- histos %>%
    tidyr::gather(bin,pct_tad, starts_with('bin')) %>%
    dplyr::rename(tad_dt=date) %>% 
    dplyr::select(one_of(c("deployid","tad_dt","bin","pct_tad"))) %>%
    dplyr::inner_join(limits) %>% 
    dplyr::select(deployid,tad_dt,bin,bin_upper_limit,pct_tad) %>% 
    dplyr::mutate(bin = factor(bin, 
                               levels = paste0("bin",1:72), 
                               ordered = TRUE)) %>% 
    dplyr::arrange(deployid,tad_dt,bin) 
  } else {
    tad_out <- histos %>%
      tidyr::gather(bin,pct_tad, starts_with('bin')) %>%
      dplyr::rename(tad_dt=date) %>% 
      dplyr::select(one_of(c("deployid","tad_dt","bin","pct_tad"))) %>%
      dplyr::select(deployid,tad_dt,bin,pct_tad) %>% 
      dplyr::mutate(bin = factor(bin, 
                                 levels = paste0("bin",1:72), 
                                 ordered = TRUE)) %>% 
      dplyr::arrange(deployid,tad_dt,bin) 
  }
  
  return(tad_out)
}
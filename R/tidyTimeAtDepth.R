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
#' @return a tibble with tidy, narrow data structure and actual
#'   time-at-depth bin limits (when provided)
#' @export
tidyTimeAtDepth <- function(histos) {
  histos_tbl <- histos$histos %>% 
    dplyr::filter(hist_type %in% c('TAD'))
  
  if (nrow(histos_tbl) == 0) {
    rlang::warn(
      glue::glue(
        "{cli::symbol$cross} No Time At Depth histogram \\
                 types found
         {cli::symbol$info} returning NULL"
      )
    )
    return(NULL)
  }
  
  limits <- histos$limits
  if (nrow(limits) == 0) {
    rlang::warn(
      glue::glue(
        "{cli::symbol$cross} Time At Depth limits not correctly specified.
         {cli::symbol$info} Will use generic bin labels."
      )
    )
  }
  if (nrow(limits) > 0) {
    limits <- dplyr::filter(histos$limits,hist_type == 'TADLIMITS') %>% 
      dplyr::select(-hist_type) %>% 
      tidyr::gather(bin,bin_upper_limit,dplyr::starts_with('bin'))
  }
  if (nrow(limits) > 0) {
  tad_out <- histos_tbl %>%
    tidyr::gather(bin,pct_tad, starts_with('bin')) %>%
    dplyr::rename(tad_start_dt=date) %>% 
    dplyr::select(one_of(c("deployid","tad_start_dt","bin","pct_tad"))) %>%
    dplyr::inner_join(limits) %>% 
    dplyr::select(deployid,tad_start_dt,bin,bin_upper_limit,pct_tad) %>% 
    dplyr::mutate(bin = factor(bin, 
                               levels = paste0("bin",1:72), 
                               ordered = TRUE)) %>% 
    dplyr::arrange(deployid,tad_start_dt,bin) 
  } else {
    tad_out <- histos_tbl %>%
      tidyr::gather(bin,pct_tad, starts_with('bin')) %>%
      dplyr::rename(tad_start_dt=date) %>% 
      dplyr::select(one_of(c("deployid","tad_start_dt","bin","pct_tad"))) %>%
      dplyr::select(deployid,tad_start_dt,bin,pct_tad) %>% 
      dplyr::mutate(bin = factor(bin, 
                                 levels = paste0("bin",1:72), 
                                 ordered = TRUE)) %>% 
      dplyr::arrange(deployid,tad_start_dt,bin) 
  }
  
  return(tad_out)
}
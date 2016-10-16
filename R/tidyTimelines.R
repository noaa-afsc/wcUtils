#' Apply 'tidy' data principles to the timeline histogram data stream
#' 
#' \code{tidyTimelines} returns a 'tidy'd' data frame of timeline data
#' 
#' The histogram data stream is provided in a 'wide' format (each row represents a time period and the observed values are provided in 1 to 72 'bin' columns). This format can be difficult to work with in R and other data analysis platforms (e.g. database tables), so we use the \code{tidyr} and \code{dplyr} packages to manipulate the data into a more flexible, 'narrow' format. This results in a data structure where every row represents a single observation.
#' 
#' This is implemented, here, with the timeline data. For timeline data, tag 'dryness' is provided as either a percentage of each hour the tag was dry or as a binary (1 or 0) value representing whether a tag was dry for a majority of a given 20-minute period. For both of these situations, the values for the 'bin' columns are predictable and we can, in addition to tidying the data structure, also turn the bin values into actual time periods
#' 
#' @param histos a list returned from \code{read_histos}
#' @param all_types by default the dominant histtype is chosen; \code{all_types = TRUE} will return records of all types found
#' 
#' @return a data frame with tidy, narrow data structure and actual time periods in place of bins
#' @export
tidyTimelines <- function(histos,all_types = FALSE) {
    histos <- histos$histos
    histos <- dplyr::filter(histos,
                            histtype %in% c('Percent','1Percent','TwentyMinTimeline'))
    if(nrow(histos) < 2) {
      warning("not enough timeline data to tidy. returning NULL")
      return(NULL)
    }
    types <- dplyr::group_by(histos,histtype)
    t <- dplyr::summarise(types, n = n())
    if (all(!t$histtype %in% c('Percent','1Percent','TwentyMinTimeline'))) {
      stop('No timeline histogram types found',call. = FALSE)
    }
    if (nrow(t) == 1) {
      type <- as.character(t$histtype[1])
    } else if (nrow(t) > 1) {
      type <- as.character(subset(t,n = max(n))$histtype[1])
    }
    if (type %in% c("Percent","1Percent") && !all_types) {
      histos_sub <- dplyr::filter(histos,
                                  histtype == type,
                                  lubridate::hour(date) == 0)
      bins <- list(bin = paste0("bin",1:24),hour = 0:23)
      bins <- as.data.frame(bins)
      timeline <- histos_sub %>%
        tidyr::gather(bin,percent_dry, starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + lubridate::hours(hour)) %>%
        dplyr::select(one_of(c("deployid","histtype","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
    }
    if (type == "TwentyMinTimeline" && !all_types) {
      histos_sub <- dplyr::filter(histos,
                                  histtype == type,
                                  lubridate::hour(date) == 0)
      bins <- list(bin = paste0("bin",1:72),
                   secs = seq(from = 0,by = 1200,length.out = 72))
      bins <- as.data.frame(bins)
      timeline <- histos_sub %>%
        tidyr::gather(bin,dry_status,starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + lubridate::seconds(secs),
                      percent_dry = ifelse(dry_status == 0,100,0)) %>%
        dplyr::select(one_of(c("deployid","histtype","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
    }
    if (all_types) {
      histos_sub <- dplyr::filter(histos,
                                  histtype %in% c("Percent","1Percent"),
                                  lubridate::hour(date) == 0)
      bins <- list(bin = paste0("bin",1:24),hour = 0:23)
      bins <- as.data.frame(bins)
      timeline1 <- histos_sub %>%
        tidyr::gather(bin,percent_dry, starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + lubridate::hours(hour)) %>%
        dplyr::select(one_of(c("deployid","histtype","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
      
      histos_sub <- dplyr::filter(histos,
                                  histtype == "TwentyMinTimeline",
                                  lubridate::hour(date) == 0)
      bins <- list(bin = paste0("bin",1:72),
                   secs = seq(from = 0,by = 1200,length.out = 72))
      bins <- as.data.frame(bins)
      timeline2 <- histos_sub %>%
        tidyr::gather(bin,dry_status,starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + lubridate::seconds(secs),
                      percent_dry = ifelse(dry_status == 0,100,0)) %>%
        dplyr::select(one_of(c("deployid","histtype","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
      timeline <- dplyr::bind_rows(timeline1,timeline2)
      
    }
  return(timeline)
}



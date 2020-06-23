#' Apply 'tidy' data principles to the timeline histogram data stream
#'
#' \code{tidyTimelines} returns a 'tidy'd' data frame of timeline data
#'
#' The histogram data stream is provided in a 'wide' format (each row represents
#' a time period and the observed values are provided in 1 to 72 'bin' columns).
#' This format can be difficult to work with in R and other data analysis
#' platforms (e.g. database tables), so we use the \code{tidyr} and \code{dplyr}
#' packages to manipulate the data into a more flexible, 'narrow' format. This
#' results in a data structure where every row represents a single observation.
#'
#' This is implemented, here, with the timeline data. For timeline data, tag
#' 'dryness' is provided as either a percentage of each hour the tag was dry or
#' as a binary (1 or 0) value representing whether a tag was dry for a majority
#' of a given 20-minute period. For both of these situations, the values for the
#' 'bin' columns are predictable and we can, in addition to tidying the data
#' structure, also turn the bin values into actual time periods.
#'
#' @param histos a list returned from \code{read_histos}
#' @param row_min user defined minimum timeline records required; \code{row_min
#'   = 2} will return NULL unless at least 1 records are found.
#'
#' @return a data frame with tidy, narrow data structure and actual time periods
#'   in place of bins
#' @export

tidyTimelines <- function(histos, row_min = 1) {
  histos <- histos$histos
  histos <- dplyr::filter(histos,
                          hist_type %in% c('Percent', '1Percent', 
                                           'TwentyMinTimeline'))
  
  if (nrow(histos) == 0) {
    rlang::warn(
      glue::glue(
        "{cli::symbol$cross} No timeline histogram \\
                 types found
         {cli::symbol$info} returning NULL"
      )
    )
    return(NULL)
  }
  if (nrow(histos) < row_min) {
    rlang::warn(
      glue::glue(
        "{cli::symbol$cross} The number of rows of \\
                 timeline data found is less than specified by `row_min`
         {cli::symbol$info} n rows found is {nrow(histos)}
         {cli::symbol$info} returning NULL"
      )
    )
    return(NULL)
  }
  
  types <- histos %>% dplyr::distinct(hist_type) %>% dplyr::pull()
  
  if (length(types) > 1) {
    rlang::warn(
      glue::glue(
        "{cli::symbol$info} more than 1 timeline type found
         {cli::symbol$info} hist_types found: {types}"
      )
    )
  }
  
  if (all(types %in% c("Percent", "1Percent"))) {
    histos_sub <- dplyr::filter(histos,
                                lubridate::hour(date) == 0)
    bins <- list(bin = paste0("bin", 1:24), hour = 0:23)
    bins <- as.data.frame(bins)
    timeline <- histos_sub %>%
      tidyr::gather(bin, percent_dry, starts_with('bin')) %>%
      merge(bins) %>%
      dplyr::mutate(timeline_start_dt = date + lubridate::hours(hour)) %>%
      dplyr::select(one_of(
        c(
          "deployid",
          "hist_type",
          "timeline_start_dt",
          "percent_dry"
        )
      )) %>%
      dplyr::arrange(deployid, timeline_start_dt)
  }
  
  if (all(types == "TwentyMinTimeline")) {
    histos_sub <- dplyr::filter(histos,
                                lubridate::hour(date) == 0)
    bins <- list(bin = paste0("bin", 1:72),
                 secs = seq(
                   from = 0,
                   by = 1200,
                   length.out = 72
                 ))
    bins <- as.data.frame(bins)
    timeline <- histos_sub %>%
      tidyr::gather(bin, dry_status, starts_with('bin')) %>%
      merge(bins) %>%
      dplyr::mutate(
        timeline_start_dt = date + lubridate::seconds(secs),
        percent_dry = ifelse(dry_status == 0, 100, 0)
      ) %>%
      dplyr::select(one_of(
        c(
          "deployid",
          "hist_type",
          "timeline_start_dt",
          "percent_dry"
        )
      )) %>%
      dplyr::arrange(deployid, timeline_start_dt)
  }
  
  if (any(types %in% c("Percent", "1Percent")) &&
      any(types == "TwentyMinTimeline")) {
    rlang::inform(
      glue::glue_col(
        "{blue {cli::symbols$info} hourly and 20-minute timeline data found"
      )
    )
    histos_sub <- dplyr::filter(histos,
                                hist_type %in% c("Percent", "1Percent"),
                                lubridate::hour(date) == 0)
    bins <- list(bin = paste0("bin", 1:24), hour = 0:23)
    bins <- as.data.frame(bins)
    timeline1 <- histos_sub %>%
      tidyr::gather(bin, percent_dry, starts_with('bin')) %>%
      merge(bins) %>%
      dplyr::mutate(timeline_start_dt = date + lubridate::hours(hour)) %>%
      dplyr::select(one_of(
        c(
          "deployid",
          "hist_type",
          "timeline_start_dt",
          "percent_dry"
        )
      )) %>%
      dplyr::arrange(deployid, timeline_start_dt)
    
    histos_sub <- dplyr::filter(histos,
                                hist_type == "TwentyMinTimeline",
                                lubridate::hour(date) == 0)
    bins <- list(bin = paste0("bin", 1:72),
                 secs = seq(
                   from = 0,
                   by = 1200,
                   length.out = 72
                 ))
    bins <- as.data.frame(bins)
    timeline2 <- histos_sub %>%
      tidyr::gather(bin, dry_status, starts_with('bin')) %>%
      merge(bins) %>%
      dplyr::mutate(
        timeline_start_dt = date + lubridate::seconds(secs),
        percent_dry = ifelse(dry_status == 0, 100, 0)
      ) %>%
      dplyr::select(one_of(
        c(
          "deployid",
          "hist_type",
          "timeline_start_dt",
          "percent_dry"
        )
      )) %>%
      dplyr::arrange(deployid, timeline_start_dt)
    timeline <- dplyr::bind_rows(timeline1, timeline2)
  }
  return(timeline)
}
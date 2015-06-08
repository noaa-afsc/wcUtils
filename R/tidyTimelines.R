#' Apply 'tidy' data principles to the timeline histogram data stream
#' 
#' \code{tidyTimelines} returns a 'tidy'd' data frame of timeline data
#' 
#' The histogram data stream is provided in a 'wide' format (each row represents a time period and the observed values are provided in 1 to 72 'bin' columns). This format can be difficult to work with in R and other data analysis platforms (e.g. database tables), so we use the \code{tidyr} and \code{dplyr} packages to manipulate the data into a more flexible, 'narrow' format. This results in a data structure where every row represents a single observation.
#' 
#' This is implemented, here, with the timeline data. For timeline data, tag 'dryness' is provided as either a percentage of each hour the tag was dry or as a binary (1 or 0) value representing whether a tag was dry for a majority of a given 20-minute period. For both of these situations, the values for the 'bin' columns are predictable and we can, in addition to tidying the data structure, also turn the bin values into actual time periods
#' 
#' @param histos an un-tidy'd histogram data frame
#' 
#' @return a data frame with tidy, narrow data structure and actual time periods in place of bins
#' @export
tidyTimelines <- function(histos) {
    types <- dplyr::group_by(histos,histtype)
    t <- dplyr::summarise(types, n=n())
    if (!t %in% c('Percent','TwentyMinTimelines')) {
      stop('No timeline histogram types found',call.=FALSE)
    }
    histos <- dplyr::filter(histos,
                            histtype %in% c('Percent','TwentyMinTimelines'))
    if(nrow(t)==1) {
      type <- as.character(t$histtype[1])
    } else if(nrow(t)>1) {
      type <- as.character(subset(t,n=max(n))$histtype[1])
    }
    if(type=="Percent") {
      histos_sub <- dplyr::filter(histos,
                                  histtype == type,
                                  lubridate::hour(date) == 0)
      bins<-list(bin=paste("bin",1:24,sep=""),hour=0:23)
      bins<-as.data.frame(bins)
      timeline <- histos_sub %>%
        tidyr::gather(bin,percent_dry, starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + lubridate::hours(hour)) %>%
        dplyr::select(one_of(c("deployid","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
    }
    if(type=="TwentyMinTimelines") {
      histos_sub <- dplyr::filter(histos,histtype == type)
      bins<-list(variable=paste("bin",1:72,sep=""),secs=seq(from=0,by=1200,length.out=72))
      bins<-as.data.frame(bins)
      timeline <- histos_sub %>%
        tidyr::gather(bin,percent_dry,starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + seconds(secs)) %>%
        dplyr::select(one_of(c("deployid","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
    }
  return(timeline)
}



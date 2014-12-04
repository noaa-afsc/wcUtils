#' Apply 'tidy' data principles to the histogram (only timelines, for now) data stream
#' 
#' \code{tidyHistos} returns a 'tidy'd' data frame of timeline data
#' 
#' The histogram data stream is provided in a 'wide' format (each row represents a time period and the observed values are provided in 1 to 72 'bin' columns). This format can be difficult to work with in R and other data analysis platforms (e.g. database tables), so we use the \code{tidyr} and \code{dplyr} packages to manipulate the data into a more flexible, 'narrow' format. This results in a data structure where every row represents a single observation.
#' 
#' This is first implemented, here, with the timeline data. For timeline data, tag 'dryness' is provided as either a percentage of each hour the tag was dry or as a binary (1 or 0) value representing whether a tag was dry for a majority of a given 20-minute period. For both of these situations, the values for the 'bin' columns are predictable and we can, in addition to tidying the data structure, also turn the bin values into actual time periods
#' 
#' @param histos an un-tidy'd histogram data frame
#' @param timelines boolean value to specify whether timelines should be processed
#' 
#' @return a data frame with tidy, narrow data structure and actual time periods in place of bins
tidyHistos <- function(histos,timelines=TRUE) {
  histos$date <- lubridate::parse_date_time(histos$date,"%H%M%S %d%m%y",tz="UTC")
  histos$source <- factor(histos$source)
  histos$instr <- factor(histos$instr)
  histos$histtype <- factor(histos$histtype)
  histos$locationquality <- factor(histos$locationquality)
  if(timelines==TRUE) {
    types <- dplyr::group_by(histos,histtype)
    t <- dplyr::summarise(types, n=n())
    if(nrow(t)==1) {
      type <- as.character(t$histtype[1])
    } else if(nrow(t)>1) {
      type <- as.character(subset(t,n=max(n))$histtype[1])
    }
    if(type=="Percent") {
      histos_sub <- subset(histos,histtype == type)
      histos_sub <- subset(histos_sub,hour(date) == 0)
      bins<-list(bin=paste("bin",1:24,sep=""),hour=0:23)
      bins<-as.data.frame(bins)
      timeline <- histos_sub %>%
        tidyr::gather(bin,percent_dry, starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + hours(hour)) %>%
        dplyr::select(one_of(c("deployid","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
    }
    if(type=="TwentyMinTimelines") {
      histos_sub <- subset(histos,histtype == type)
      bins<-list(variable=paste("bin",1:72,sep=""),secs=seq(from=0,by=1200,length.out=72))
      bins<-as.data.frame(bins)
      timeline <- histos_sub %>%
        tidyr::gather(bin,percent_dry,starts_with('bin')) %>%
        merge(bins) %>%
        dplyr::mutate(datadatetime = date + seconds(secs)) %>%
        dplyr::select(one_of(c("deployid","datadatetime","percent_dry"))) %>%
        dplyr::arrange(deployid,datadatetime)
    }
  }
  return(timeline)
}



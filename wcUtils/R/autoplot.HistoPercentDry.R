#' autoplot.HistoPercentDry
#'
#' 
#' @param d dataframe object output from MeltHistos function
#' @return ggplot
#' @note future plans to evolve this to an autoplot function
#' @author Josh M London \email{josh.london@@noaa.gov}
#' @export
#' @import ggplot2
autoplot.HistoPercentDry<- function(d, ..., xlab="", ylab="", title="Percent Dry Timeline") {
  d$Hour <- as.POSIXlt(d$DataDateTime,"GMT")$hour
  d$DayOfMonth <- as.POSIXlt(d$DataDateTime,"GMT")$mday
  d$Month <- as.POSIXlt(d$DataDateTime,"GMT")$mon+1
  d$Year <- as.POSIXlt(d$DataDateTime,"GMT")$year+1900
  p<-ggplot(d,aes(DayOfMonth,Hour)) + geom_tile(aes(fill = PercentDry)) + 
    scale_fill_gradient2(low="#2166AC",mid="ghostwhite",high="#B2182B",
                         midpoint=50,na.value="grey80") + 
    facet_grid(Year+Month ~ DeployID)
  print(p)
}
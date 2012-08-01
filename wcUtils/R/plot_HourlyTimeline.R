#' plot_HourlyTimeline
#'
#' Plot hourly timeline data
#' @param d dataframe object output from MeltHistos function
#' @return ggplot
#' @note future plans to evolve this to an autoplot function
#' @author Josh M London \email{josh.london@@noaa.gov}
#' @export
#' @import ggplot2
plot_HourlyTimeline <- function(d) {
  require(ggplot2)
  d$Hour <- as.POSIXlt(d$DataDateTime,"GMT")$hour
  d$DayOfMonth <- as.POSIXlt(d$DataDateTime,"GMT")$mday
  d$Month <- as.POSIXlt(d$DataDateTime,"GMT")$mon+1
  d$Year <- as.POSIXlt(d$DataDateTime,"GMT")$year+1900
  p<-ggplot(d,aes(DayOfMonth,Hour)) + geom_tile(aes(fill = PercentDry)) + 
    scale_fill_gradient(low = "#F7FBFF", high = "#08306B") + 
    facet_grid(Year+Month ~ DeployID)
  print(p)
}
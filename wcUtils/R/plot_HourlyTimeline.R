plot_HourlyTimeline <- function(d) {
  d$Hour <- as.POSIXlt(d$DataDateTime,"GMT")$hour
  d$DayOfMonth <- as.POSIXlt(d$DataDateTime,"GMT")$mday
  d$Month <- as.POSIXlt(d$DataDateTime,"GMT")$mon+1
  d$Year <- as.POSIXlt(d$DataDateTime,"GMT")$year+1900
  p<-ggplot(d,aes(DayOfMonth,Hour)) + geom_tile(aes(fill = PercentDry)) + 
    scale_fill_gradient(low = "#F7FBFF", high = "#08306B") + 
    facet_grid(Year+Month ~ DeployID)
  print(p)
}
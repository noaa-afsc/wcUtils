plot_HourlyTimeline <- function(d) {
  d$Hour <- as.POSIXlt(drytimes$DataDateTime,"GMT")$hour
  drytimes$DayOfMonth <- as.POSIXlt(drytimes$DataDateTime,"GMT")$mday
  drytimes$Month <- as.POSIXlt(drytimes$DataDateTime,"GMT")$mon+1
  p<-ggplot(drytimes,aes(DayOfMonth,Hour)) + geom_tile(aes(fill = PercentDry)) + 
    scale_fill_gradient(low = "#F7FBFF", high = "#08306B") + 
    facet_grid(Month ~ DeployID)
  print(p)
}
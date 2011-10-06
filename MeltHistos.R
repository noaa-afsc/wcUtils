#FUNCTION TO PREP DATA FROM THE *-Histos.csv FILES OUTPUT FROM WC-DAP FOR
#IMPORT INTO CRAWL AS HAUL-OUT DATA

MeltHistos <- function(d,hist_type="Percent") {
  library(reshape)
  library(lubridate)
  histos<-read.csv(d)
  histos_sub<-histos[histos$HistType == hist_type,]
  ind <- sapply(histos_sub, is.factor)
  histos_sub[ind] <- lapply(histos_sub[ind], "[", drop=TRUE)
  if(hist_type == 'Percent') {
    drydata<-per_sub[,c(1,7,16:39)]
    drydata<-melt(drydata,id=1:2)
  }
  drydata$DateTime <- ymd_hms(drydata$Date)
}


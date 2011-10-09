#FUNCTION TO PREP DATA FROM THE *-Histos.csv FILES OUTPUT FROM WC-DAP FOR
#IMPORT INTO CRAWL AS HAUL-OUT DATA

MeltHistos <- function(d,hist_type="Percent") {
  library(reshape)
  library(lubridate)
  bins<-data.frame(cbind(variable=paste("Bin",1:24,sep=""),hrs=0:23))
  bins$hrs<-as.numeric(bins$hrs)
  histos<-read.csv(d)
  histos_sub<-histos[histos$HistType == hist_type,]
  ind <- sapply(histos_sub, is.factor)
  histos_sub[ind] <- lapply(histos_sub[ind], "[", drop=TRUE)
  if(hist_type == 'Percent') {
    drydata<-histos_sub[,c(1,7,16:39)]
    drydata<-melt(drydata,id.vars=1:2)
  }
  drydata<-merge(drydata,bins)
  drydata$DateTime<-as.POSIXct(strptime(drydata$Date, "%Y-%m-%d %H:%M:%S",tz="UTC"),tz="UTC") + 
    dhours(drydata$hrs)
  drydata$DateTime<-force_tz(drydata$DateTime)
  drydata<-drydata[,c(2,6,4)]
  names(drydata)<-c("DeployID","DataDateTime","PercentDry")
  drydata<-drydata[with(drydata, order(DeployID, DataDateTime)), ]
  return(drydata)
}


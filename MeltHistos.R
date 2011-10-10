MeltHistos <- function(d,hist_type="Percent") {
  # function for 'melting' histos data output from Wildlife Computers' 
  # WC-DAP program. Stringent requirements exist for *-Histos.csv file 
  # specified as input.
  # Notes:
  # -Format of Date column must be YYYY-MM-DD HH:MM:SS
  # -CSV file should not be edited.
  # -Currently, only drytime data (e.g. 'Percent') is supported
  # -Requires reshape package
  require(reshape)
  histos<-read.csv(d)  # read in the specified *-Histos.csv file
  histos_sub<-histos[histos$HistType == hist_type,] #subset by hist_type
  # after subsetting, need to drop levels.
  ind <- sapply(histos_sub, is.factor)
  histos_sub[ind] <- lapply(histos_sub[ind], "[", drop=TRUE)
  # melt and process procedures for 'Percent' drytime histos
  if(hist_type == 'Percent') {
    bins<-data.frame(cbind(variable=paste("Bin",1:24,sep=""),hrs=0:23))
    bins$hrs<-as.numeric(bins$hrs)
    drydata<-histos_sub[,c(1,7,16:39)]
    drydata<-melt(drydata,id.vars=1:2)
    drydata<-merge(drydata,bins)
    drydata$DateTime<-as.POSIXct(strptime(drydata$Date, "%Y-%m-%d %H:%M:%S",
                                          tz="UTC"),tz="UTC") + drydata$hrs*3600
    drydata$DateTime<-force_tz(drydata$DateTime) # force timezone to UTC
    drydata<-drydata[,c(2,6,4)]
    names(drydata)<-c("DeployID","DataDateTime","PercentDry")
  }
  drydata<-drydata[with(drydata, order(DeployID, DataDateTime)), ]
  return(drydata)
}


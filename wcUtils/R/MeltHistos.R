#' MeltHistos
#'
#' Melt XX-Histos.csv data to a more vertical data structure
#'
#' One of the main reasons for deploying Wildlife Computers tags is to gather 
#' information related to dive and haul-out behavior. In order to work within 
#' the bandwidth limitations of Argos, Wildlife Computers tags typically transmit 
#' dive behavior back to the researcher in the form of histograms. Various dive 
#' data are collected in this manner and include 'Dive Depth','Time At Depth', 
#' 'Time At Temperature' and 'Percent Dry'. In the case of the 'Percent Dry' data, 
#' each record represents a single UTC day and each 'Bin' column represents an 
#' hour of that day ('Bin1' = 00:00, 'Bin2' = 01:00, ... 'Bin24 = 23:00). For data 
#' related to dive behavior, each 'Bin' column represents a range of depths or time 
#' durations specified by the user at the time of tag programming. Each record 
#' represents a specific duration of time (e.g. 6 hour period) also specified by 
#' the user. All of these data are represented in the *-Histos.csv output from 
#' WC-DAP and the data structure is organized horizontally.
#'
#' Often, it is desirable for the data to be represented in a more vertical nature 
#' where each record specifies a single hour of a day (for 'Percent Dry' data) or a 
#' specific Bin range for dive data. This vertical structure is more easily imported 
#' into relational databases or other analysis functions. Re-shaping (in this case 
#' 'melting') the data into this vertical structure is the purpose of this function.
#' 
#' Initially, the 'MeltHistos' function has been written to process only those 
#' histogram data related to haul-out behavior. These records are identified within 
#' the -Histos.csv' as having a HistType of 'Percent' (or, in the rarer case 
#' 'TwentyMinTimeline'). This function requires the user to provide the path to the 
#' -Histos.csv' file and it returns a dataframe with three columns: DeployID, 
#' DataDateTime and PercentDry. All time values are in the UTC time zone.
#'
#' @param d path to *-Histos.csv as a string
#' @param hist_type the type of histos data to melt. default is 'Percent'
#' @return dataframe with three columns: DeployID,DataDateTime and PercentDry
#' @note currently only works for 'Percent' records. Also the 
#' format of the Date column must be YYYY-MM-DD HH:MM:SS and the csv file must not
#' have been edited.
#' @author Josh M London \email{josh.london@@noaa.gov}
#' @export
#' @import reshape
#' @examples
#' example_csv<-system.file("sealdata.csv", package = "wcUtils") 
#' drytimes<-MeltHistos(d=example_csv,hist_type='Percent')
MeltHistos <- function(d,hist_type="Percent") {
  require(reshape)
  histos<-read.csv(d)
  histos_sub<-histos[histos$HistType == hist_type,]
  histos_sub<-histos_sub[as.POSIXlt(as.POSIXct(strptime(histos_sub$Date, "%Y-%m-%d %H:%M:%S")))$hour == 0,]
  ind <- sapply(histos_sub, is.factor)
  histos_sub[ind] <- lapply(histos_sub[ind], "[", drop=TRUE)
  if(hist_type == 'Percent') {
    bins<-list(variable=paste("Bin",1:24,sep=""),hrs=0:23)
    bins<-as.data.frame(bins)
    drydata<-histos_sub[,c(1,7,16:39)]
    drydata<-melt(drydata,id.vars=1:2)
    drydata<-merge(drydata,bins)
    drydata$DateTime<-as.POSIXct(strptime(drydata$Date, "%Y-%m-%d %H:%M:%S",
                                          tz="UTC"),tz="UTC") + drydata$hrs*3600
    drydata<-drydata[,c(2,6,4)]
    names(drydata)<-c("DeployID","DataDateTime","PercentDry")
  }
  if(hist_type == 'TwentyMinTimeline') {
      bins<-list(variable=paste("Bin",1:72,sep=""),secs=seq(from=0,by=1200,length.out=72))
      bins<-as.data.frame(bins)
      drydata<-histos_sub[,c(1,7,16:87)]
      drydata<-melt(drydata,id.vars=1:2)
      drydata<-merge(drydata,bins)
      drydata$DateTime<-as.POSIXct(strptime(drydata$Date, "%Y-%m-%d %H:%M:%S",
                                            tz="UTC"),tz="UTC") + drydata$secs
      drydata<-drydata[,c(2,6,4)]
      names(drydata)<-c("DeployID","DataDateTime","PercentDry")
  }
  drydata<-drydata[with(drydata, order(DeployID, DataDateTime)), ]
  #this is where we add in NA's for missing data
  #first get the start/end times for each seal and store in a list
  ll <- vector(mode="list",length=length(levels(drydata$DeployID)))
  for(i in 1:length(levels(drydata$DeployID))) {
    ll[[i]] <- list(start=min(drydata$DataDateTime[drydata$DeployID==levels(drydata$DeployID[i])]),
                    finish=max(drydata$DataDateTime[drydata$DeployID==levels(drydata$DeployID[i])]))
  }
  names(ll) <- levels(drydata$DeployID)
  fulldata <- NULL
  diff <- ifelse(hist_type=="Percent","1 hour","20 mins")
  for(j in names(ll)) {
    fulldata<-rbind(fulldata, data.frame(DeployID=j,
    DataDateTime=seq(from=ll[[j]]$start,to=ll[[j]]$finish,by=diff)))
  }
  fulldata <- merge(fulldata,drydata,all.x=TRUE)
  return(fulldata)
}


#' Melt -Histos.csv data to a more vertical data structure
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
  ind <- sapply(histos_sub, is.factor)
  histos_sub[ind] <- lapply(histos_sub[ind], "[", drop=TRUE)
  if(hist_type == 'Percent') {
    bins<-data.frame(cbind(variable=paste("Bin",1:24,sep=""),hrs=0:23))
    bins$hrs<-as.numeric(bins$hrs)
    drydata<-histos_sub[,c(1,7,16:39)]
    drydata<-melt(drydata,id.vars=1:2)
    drydata<-merge(drydata,bins)
    drydata$DateTime<-as.POSIXct(strptime(drydata$Date, "%Y-%m-%d %H:%M:%S",
                                          tz="UTC"),tz="UTC") + drydata$hrs*3600
    drydata<-drydata[,c(2,6,4)]
    names(drydata)<-c("DeployID","DataDateTime","PercentDry")
  }
  drydata<-drydata[with(drydata, order(DeployID, DataDateTime)), ]
  return(drydata)
}


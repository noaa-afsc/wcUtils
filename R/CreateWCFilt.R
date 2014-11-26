#' CreateWCFilt
#'
#' Create a *.wcfilt file from a dataframe of deployment information
#'
#' The Wildlife Computers' WC-DAP program requires the creation of 'filters'
#' to organize PTT deployments within an Argos program. These filters can be
#' created within the WC-DAP GUI interface, but this can be tedious for large
#' numbers of deployments and there is no method for importing this data. Often
#' researchers have data on deployments already organized into a spreadsheet,
#' database or flat file. This function uses the XML package within R to 
#' process a dataframe into an XML file compatible with the WC-DAP filter
#' file (*.wcfilt).
#'
#' @param tagdata a dataframe with columns 'Name', 'Ptt', and 'Start'
#' @param projectid a string specifying the name for the project or filter
#' @param dateformat a string describing the dateformat
#' @param minclass a string value of either "B","A","0","1","2", or "3"
#' @return an xml formated output that can be written to a .wcfilt file
#' @export
#' @note future plans to have the function accept a destination path and
#' actually write the output file.
#'
CreateWCFilt <- function(tagdata,projectid,
                         dateformat="Y-M-D h:m:s",minclass="B") {
  require(XML)
  thisxml <- xmlTree()
  thisxml$addTag("WildlifeComputers",close=FALSE)
  thisxml$addTag("Project",close=FALSE)
  thisxml$addTag("Name",projectid)
  thisxml$addTag("DateFormat",dateformat)
  thisxml$addTag("MinArgosClass",minclass)
  
  for(i in 1:nrow(tagdata)) {
    thisxml$addTag("Instrument",close=FALSE)
    for (j in c("Name","Ptt","Start")) {
      thisxml$addTag(j,tagdata[i, j])
    }
    thisxml$closeTag()
  }
  thisxml$closeTag()
  thisxml$closeTag()
  return(thisxml)
}
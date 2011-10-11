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
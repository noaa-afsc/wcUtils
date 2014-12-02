#' Return a vector of deployment IDs with new data in the last \code{n} days
#' 
#' \code{wcGetProjectIDs} returns a vector of deployment IDs
#' 
#' This returns a subset of deployment IDs with new data available on the portal within the last
#' \code{n} days. The default value is for 14 days
#' 
#' @param xml_content XML content/data returned from wcPOST (with 'action=get_deployments')
#' @param days integer value specifying the time window from \code{now()} in days
#'   
#' @return returns a vector of deployment IDs
wcGetRecentIDs <- function(xml_content,days=14) {
  if(class(xml_content) == "response") {
    warning('wcPOST response object provided, extracting content to xml',call.=FALSE)
    xml_content <- httr::content(xml_content)
  }
  start_epoch <- as.numeric(now()-lubridate::days(14))
  xpath <- paste("//data/deployment/argos/last_uplink_date",
                 "[number() > number(",
                 start_epoch,
                 ")]/../../id",sep="")
  ids <- XML::xpathSApply(xml_content,xpath,xmlValue)
  return(ids)
}

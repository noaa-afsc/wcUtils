#' Return a vector of deployment IDs
#' 
#' \code{wcGetIDs} returns a vector of deployment IDs
#' 
#' Each deployment in the Wildlife Computers Data Portal is identified by a unique alpha-numeric value. This function searches the XML response data and extracts those IDs
#' 
#' @param xml_content XML content/data returned from wcPOST (with 'action=get_deployments')
#' @param xpath additional customization possible by passing an xpath statement
#' 
#' @return returns a vector of deployment IDs
#' @export
wcGetIDs <- function(xml_content,xpath=NULL) {
  if(any(class(xml_content) == "response")) {
    warning('wcPOST response object provided, extracting content to xml',call.=FALSE)
    xml_content <- httr::content(xml_content)
  }
  
  if(!is.null(xpath)) {
    xpath<-xpath
  } else {
    xpath <- "//data/deployment/id"
  }
  ids <- XML::xpathSApply(xml_content,xpath,XML::xmlValue)
  return(ids)
}
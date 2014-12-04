#' Process GPS/fastloc location data
#' 
#' \code{wcProcessGPS} determines the GPS/Fastloc tags in need of processing and sends a command to
#' initiate processing
#' 
#' @param xml_content XML content/data returned from wcPOST (with 'action=get_deployments')
#' 
#' @return nothing is returned
#' @export
wcProcessGPS <- function(xml_content) {
  if(any(class(xml_content) == "response")) {
    warning('wcPOST response object provided, extracting content to xml',call.=FALSE)
    xml_content <- httr::content(xml_content)
  }
  xpath <- paste("//data/deployment/status[text() = 'needs processing']/../id")
  ids_to_process <- wcGetIDs(xml_content,xpath)
  xpath <- paste("//data/deployment/status[text() = 'updating']/../id")
  ids_to_wait <- wcGetIDs(xml_content,xpath)
  if(length(ids_to_process) == 0 & length(ids_to_wait) == 0) {
    stop("No deployments are in need of GPS processing")
  }
  while(length(ids_to_wait) > 0) {
    xpath <- paste("//data/deployment/status[text() = 'updating']/../id")
    ids_to_wait <- wcGetIDs(xml_content,xpath)
    Sys.sleep(5)
  }
  for(i in 1:length(ids_to_process)){
    update_params <- paste("action=update_deployment&id=",
                           ids_to_process[i],
                           "&request=process",sep="")
    wcPOST(params=update_params)
  }
  while(length(ids_to_wait) > 0) {
    xml_content <- httr::content(wcPOST())
    xpath <- paste("//data/deployment/status[text() = 'updating']/../id")
    ids_to_wait <- wcGetIDs(xml_content,xpath)
    Sys.sleep(5)
  }
}
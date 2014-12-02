#' Return a vector of deployment IDs associated with a given Project
#' 
#' \code{wcGetProjectIDs} returns a vector of deployment IDs
#' 
#' This function presumes a custom label,'Project', has been setup for deployments on the Wildife
#' Computers Data Portal. The vector of deployment IDs returned will be a subset that match the
#' project name provided in the function call.
#' 
#' @param xml_content XML content/data returned from wcPOST (with 'action=get_deployments')
#' @param project valid project name (required)
#'   
#' @return returns a vector of deployment IDs
wcGetProjectIDs <- function(xml_content,project=NULL) {
  if(class(xml_content) == "response") {
    warning('wcPOST response object provided, extracting content to xml',call.=FALSE)
    xml_content <- httr::content(xml_content)
  }
  if(is.null(project)) {
    stop("Error: you must provide a project name")
  }
  valid_project <- 
    XML::getNodeSet(doc,paste("boolean(//data/deployment/labels/category",
                         "/name[text() = 'Project'])",sep=""))
  if(!valid_project) {
    stop("Error: no 'Project' label found")
  }
  xpath <- paste("//data/deployment/labels/category/",
                 "name[text() = 'Project']/../label[text() = '",
                  project,
                 "']/../../../id",sep="")
  ids <- XML::xpathSApply(xml_content,xpath,xmlValue)
  return(ids)
}
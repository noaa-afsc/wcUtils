#' Return a vector of data portal unique IDs associated with a given DeployID
#' 
#' \code{wcGetDeployID} returns a vector of data portal unique ID(s)
#' 
#' This function presumes a DeployID has been setup for deployments on the Wildife
#' Computers Data Portal. The vector of deployment ID(s) returned will be a subset that match the
#' deployid character in the function call. The list returned will also include a simple
#' data frame with summary information one can use to determine the appropriate id.
#' 
#' @param xml_content XML content/data returned from wcPOST (with 'action=get_deployments')
#' @param deployid valid deployid character (required)
#'   
#' @return a list with ids (a vector of deployment ids) and a df (data frame of deployment summaries)
#' @export
wcGetDeployID <- function(xml_content,deployid=NULL) {
  if(class(xml_content) == "response") {
    warning('wcPOST response object provided, extracting content to xml',call.=FALSE)
    doc <- xml2::read_xml(httr::content(xml_content, 'raw'))
  } else if(class(xml_content) == "xml_document") {
    doc <- xml_content
  } else {
    stop('xml_content not a valid response or xml_document')
  }
  if(is.null(deployid)) {
    stop("you must provide a valid deployid")
  } else {
    deployid <- as.character(deployid)
  }
  # extract the individual rows
  rows <-
    xml2::xml_find_all(doc, paste0("//deployment[deploy_id = '",deployid,"']"))
  if (length(rows) < 1) {
    warning('no deployment record was found')
  }
  if (length(rows) > 1) {
    warning('more than one deployment record found')
  }
  # create  summary data frame for each deployment
  stor_list <- vector(mode = 'list', length = length(rows))
  for (i in 1:length(rows)) {
    stor_list[[i]] <- data.frame(
      deployid = ifelse(length(xml2::xml_text(
        xml2::xml_find_all(rows[[i]], './deploy_id')))==0,
                        NA,
        xml2::xml_text(xml2::xml_find_all(rows[[i]],'./deploy_id'))),
      argos_program = xml2::xml_text(
        xml2::xml_find_all(rows[[i]], './argos/program_number')),
      ptt = xml2::xml_text(
        xml2::xml_find_all(rows[[i]], './argos/ptt_decimal')),
      argos_first = as.POSIXct(
        as.numeric(xml2::xml_text(xml2::xml_find_all(
        rows[[i]], './argos/first_uplink_date'))),
        origin = '1970-01-01 00:00:00'),
      argos_last = as.POSIXct(
        as.numeric(xml2::xml_text(xml2::xml_find_all(
        rows[[i]], './argos/last_uplink_date'))),
        origin = '1970-01-01 00:00:00'),
      id = xml2::xml_text(xml2::xml_find_all(rows[[i]], './id')),
      stringsAsFactors = FALSE
    )
  }
  df <- dplyr::bind_rows(stor_list)
  ids <- list(ids = df$id,df=df)
  return(ids)
}
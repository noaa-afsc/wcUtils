wcProcessGPS <- function(doc) {
  xpath <- paste("//data/deployment/status[text() = 'needs processing']/../id")
  ids_to_process <- wcGetIDs(doc,xpath)
  if(ids_to_process<1) {
    stop("No deployments are in need of GPS processing")
  }
  for(i in 1:length(ids_to_process)){
    update_params <- paste("action=update_deployment&id=",
                           ids_to_process[i],
                           "&request=process",sep="")
    wcPOST(params=update_params)
  }
}
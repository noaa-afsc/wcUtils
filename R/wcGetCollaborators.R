wcGetCollaborators <- function(wc.key=Sys.getenv("WCACCESSKEY"),
                   wc.secret=Sys.getenv("WCSECRETKEY"),
                   keyfile=NULL,
                   params="action=get_collaborators") {
  
  if (!is.null(keyfile)) {
    keys <- jsonlite::fromJSON(keyfile)
    wc.key <- keys$WCACCESSKEY
    wc.secret <- keys$WCSECRETKEY
  }
  
  if (is.null(wc.key) | is.null(wc.secret)) {
    stop("Wildlife Computers keys not found. 
         Either use .Renviron (see help) or a keyfile.json")
  }
  
  x.hash <- digest::hmac(wc.secret,params,algo = "sha256")
  
  r <- httr::POST("http://my.wildlifecomputers.com/services/",
                  body = params,
                  httr::add_headers("X-Access" = wc.key,"X-Hash" = x.hash))
  return(r)
}
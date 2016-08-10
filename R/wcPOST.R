#' Send POST to Wildlife Computers Data Portal API
#' 
#' \code{wcPOST} returns a response from a POST to the API
#' 
#' This function provides basic access to the API via POST. The \code{params} value contains the 
#' string to include in the body of the POST. The default action is to return a list of deployments 
#' associated with your account . Most users will likely not call this function directly, but 
#' instead, rely on other helper/wrapper functions within the package.
#' 
#' The Wildlife Computers Data Portal API uses a form of keyed-hash message authentication code for 
#' secure access. Your 'Access Key' and 'Secret Key' can be obtained from the data portal website 
#' (Account Settings > Web Services Security). For security reasons you should NOT include the keys 
#' as plain text in any scripts. Instead, include the key values as within your 
#' \code{.Renviron}.
#' 
#' @section Setting key values within .Renviron: 
#'   This option is a preferred option for storing keys,
#'   passwords and other sensitive values. Your \code{.Renviron} should be secured via OS
#'   security/permissions (e.g. on Linux/OS X \code{.Renviron} is stored within the home directory
#'   which is only accessible by an authorized user. However, you should not share your
#'   \code{.Renviron} or include your \code{.Renviron} in version control (e.g. git) if you use this
#'   option. An alternative is to read values from a different file in the home directory or to use
#'   OS level environment variables.
#'   
#'   preformatted{ wcAccessKey = 'E4iZhsfdje7590JDNR/VARTEZyhfwb84485X5Xw86ow=' 
#'   wcSecretKey = 'WIRJFYhfjdsuSEqKoE7WSDvXUHzVP0pHDJSscmeA7fw=' }
#'   
#' @param wc.key public access key (default retrieves from option value set in .Renviron)
#' @param wc.secret secret access key (default retrieves from option value set in .Renviron)
#' @param keyfile path to a json formatted keyfile with wcAccessKey and wcSecretKey
#' @param params POST message (default returns a list of deployments)
#'   
#' @return an \code{httr} response object is returned. Content of the response can be obtained with
#'   the \code{httr::content()} function.
#' @export
wcPOST <- function(wc.key=Sys.getenv("wcAccessKey"),
                   wc.secret=Sys.getenv("wcSecretKey"),
                   keyfile=NULL,
                   params="action=get_deployments") {
  
  if (!is.null(keyfile)) {
    keys <- jsonlite::fromJSON(keyfile)
    wc.key <- keys$wcAccessKey
    wc.secret <- keys$wcSecretKey
  }
  
  if (is.null(wc.key) | is.null(wc.secret)) {
    stop("Wildlife Computers keys not found. Either use .Renviron (see help) or a keyfile.json")
  }
  
  x.hash <- digest::hmac(wc.secret,params,algo = "sha256")
  
  r <- httr::POST("http://my.wildlifecomputers.com/services/",
            body = params,
            httr::add_headers("X-Access" = wc.key,"X-Hash" = x.hash))
  return(r)
}

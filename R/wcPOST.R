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
#' as plain text in any scripts. Instead, include the key values as options within your 
#' \code{.Rprofile}.
#' 
#' @section Setting key values within .Rprofile: 
#'   This option is a preferred option for storing keys,
#'   passwords and other sensitive values. Your \code{.Rprofile} should be secured via OS
#'   security/permissions (e.g. on Linux/OS X \code{.Rprofile} is stored within the home directory
#'   which is only accessible by an authorized user. However, you should not share your
#'   \code{.Rprofile} or include your \code{.Rprofile} in version control (e.g. git) if you use this
#'   option. An alternative is to read values from a different file in the home directory or to use
#'   environment variables.
#'   
#'   preformatted{ options(wcAccessKey = 'E4iZhsfdje7590JDNR/VARTEZyhfwb84485X5Xw86ow=') 
#'   options(wcSecretKey = 'WIRJFYhfjdsuSEqKoE7WSDvXUHzVP0pHDJSscmeA7fw=') }
#'   
#' @param wc.key public access key (default retrieves from option value set in .Rprofile)
#' @param wc.secret secret access key (default retrieves from option value set in .Rprofile)
#' @param params POST message (default returns a list of deployments)
#'   
#' @return an \code{httr} response object is returned. Content of the response can be obtained with
#'   the \code{httr::content()} function.
#' @export
wcPOST <- function(wc.key=getOption("wcAccessKey"),
                   wc.secret=getOption("wcSecretKey"),
                   params="action=get_deployments") {
  
  x.hash <- digest::hmac(wc.secret,params,algo="sha256")
  
  r <- httr::POST("http://my.wildlifecomputers.com/services/",
            body=params,
            httr::add_headers("X-Access" = wc.key,"X-Hash" = x.hash))
  return(r)
}

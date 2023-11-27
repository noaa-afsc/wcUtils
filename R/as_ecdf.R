#' Create An ECDF List
#'
#' This creates a standardized list for holding ECDF data and sets the class
#' to `ecdf`. The S3 class, *wcECDF* consists of three named objects: `type`,
#' `percent_time`, and `ecdf`. The `type` object is the same as that specified
#' for the `type` parameter. The `percent_time` object holds the proportion of
#' the summary period the tag spent within the `shallow` or `deep` portion of
#' the water column. Lastly, the `ecdf` object is a data.frame with a column,
#' `ecd_prop`, that specifies the proportional values for the ECDF and a
#' column, `depth_break`, that reports the corresponding depth value (in meters).
#'
#' @param type this is either `shallow` or `deep`
#' @param ecdf a data.frame
#'
#' @return A list with three named objects (`type`,`percent_time`,`ecdf`) of
#'     class `wcECDF`
#' @export
#'
as_ecdf <- function(type, ecdf) {
  if (is.null(ecdf)) {
    return(NULL)
  }
  if (missing(type)) {
    stop("type must be specified.")
  }
  if (!type %in% c('shallow','deep')) {
    stop(
      paste("type must be specified as either 'shallow' or 'deep'.",
            "in this case,", type, "was provided")
    )
  }
  
  percent_time <- mean(ecdf$percent_time) * 0.01
  
  ecdf <- ecdf[!is.na(ecdf$depth_break),]
  
  ecdf$ecd_prop <- ecdf$ecd_pct * 0.01
  ecdf <- drop_ecd_duplicates(ecdf)
  
  if(suppressWarnings(min(ecdf$depth_break) > 0)) {
    ecdf <- tibble::add_row(ecdf, depth_break = 0, ecd_prop = 0, .before = 1)
  }
  
  if(is.unsorted(ecdf$depth_break)) {
    cli::cli_warn(c("Depth breaks are unsorted and must be increasing in value",
                    i = "empty value returned"))
    ecdf_s3 <- list(
      type = type,
      percent_time = percent_time,
      ecdf = c()
    )
    class(ecdf_s3) <- "wcECDF"
    return(ecdf_s3)
  }
  
  ecdf_s3 <- list(
    type = type,
    percent_time = percent_time,
    ecdf = ecdf[,c("ecd_prop","depth_break")]
  )
  class(ecdf_s3) <- "wcECDF"
  return(ecdf_s3)
}

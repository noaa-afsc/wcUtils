#' Read and Tidy ECDF Data File
#'
#' This function is the workhorse of the `wcECDF` package. All ECDF data are
#' stored within a `*-ECDHistos.csv` file that is output from either the
#' Wildlife Computers Data Portal or DAP processing software. The function
#' presumes the `*-ECDHistos.csv` data file is provided as-is from these sources
#' and has not been edited. The resulting output is a nested `tibble` that
#' adheres to tidy data principles and includes new columns (`shallow_ecdf`,
#' `deep_ecdf`, `full_ecdf`, and `full_pdf`.
#'
#' In addition to *tidying* up the original data into a more workable *long*
#' format, this function calculates four new columns.
#'
#' **shallow_ecdf**
#' The `shallow_ecdf` column is a list-col that contains nested S3 objects of
#' class `wcECDF` representing the portion of the water column defined as `shallow`.
#'
#' **deep_ecdf**
#' The `deep_ecdf` column is a list-col that contains nested S3 objects of class
#' `wcECDF` representing the portion of the water column defined as `deep`.
#'
#' **full_ecdf**
#' The combined ECDF for both shallow and deep regions. The resulting ECDF is
#' weighted based on the reported proportion of time spent within each region.
#'
#' **full_pdf**
#' The `full_ecdf` is transformed into a probability density function and two
#' columns are returned: `pdf` and `prob`. The later represents the probability
#' the tag spent time at a given depth.
#'
#' @param ecdf_csv file path for the `*-ECDHistos.csv`
#'
#' @return A nested tibble
#' @export
#'
read_ecdf <- function(ecdf_csv) {
  # column type specifications for ECDHistos.csv
  .cols <- readr::cols(
    .default = readr::col_double(),
    DeployID = readr::col_character(),
    Ptt = readr::col_integer(),
    Source = readr::col_character(),
    Instr = readr::col_character(),
    Start = readr::col_character(),
    End = readr::col_character(),
    Kind = readr::col_character(),
    Type = readr::col_character()
  )
  
  .orders <- c("dmY HMS",
               "Ymd HMS",
               "HMS dbY",
               "dbY HMS")
  
  ecdhistos <-
    readr::read_csv(ecdf_csv, col_types = .cols) %>%
    dplyr::mutate(
      Start = lubridate::parse_date_time(Start, .orders),
      End = lubridate::parse_date_time(End, .orders)
    ) %>%
    janitor::clean_names(case = "snake") %>%
    dplyr::select(-c(min_sec, max_sec)) %>%
    dplyr::arrange(deploy_id, start) %>%
    # begin pivot of data to longer, tidy format
    tidyr::pivot_longer(
      cols = c(min_depth, percent_20:percent_100),
      names_to = "ecd_pct",
      values_to = "depth_break"
    ) %>%
    dplyr::mutate(
      ecd_pct = dplyr::case_when(
        ecd_pct == "min_depth" ~ 0.0,
        ecd_pct == "percent_20" ~ 20.0,
        ecd_pct == "percent_33" ~ 33.0,
        ecd_pct == "percent_40" ~ 40.0,
        ecd_pct == "percent_60" ~ 60.0,
        ecd_pct == "percent_66" ~ 66.0,
        ecd_pct == "percent_80" ~ 80.0,
        ecd_pct == "percent_100" ~ 100.0
      )
    ) %>%
    dplyr::group_by(deploy_id, ptt, source, instr, start, end, kind, type) %>%
    dplyr::arrange(ecd_pct, .by_group = TRUE) %>%
    tidyr::nest() %>%
    tidyr::pivot_wider(names_from = type, values_from = data) %>%
    dplyr::mutate(dry = purrr::map_dbl(dry,
                                       ~ purrr::pluck(., "percent_time", 1,
                                                      .default = 0.0))) %>%
    dplyr::rename(percent_dry = dry) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(shallow_ecdf = purrr::map2("shallow",shallow, as_ecdf),
                  deep_ecdf = purrr::map2("deep",deep, as_ecdf)) %>%
    dplyr::mutate(full_ecdf = purrr::map2(shallow_ecdf, deep_ecdf,
                                          combine_ecdf, return = "ecdf")) %>% 
    dplyr::select(-c(shallow,deep))
}

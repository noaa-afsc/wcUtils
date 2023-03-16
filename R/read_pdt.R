#' Read and Tidy PDT Data File
#'
#' All PDT data are
#' stored within a `*-PDTs.csv` file that is output from either the
#' Wildlife Computers Data Portal or DAP processing software. The function
#' presumes the `*-PDTs.csv` data file is provided as-is from these sources
#' and has not been edited. The resulting output is a `tibble` that
#' adheres to tidy data principles and includes five columns (`deploy_id`,
#' `date`, `depth`, `min_te`, and `max_te`.
#'
#'
#' @param pdt_csv file path for the `*-PDTs.csv`
#'
#' @return A tibble
#' @export
#'
#'

.orders <- c("dmY HMS",
             "Ymd HMS",
             "HMS dbY",
             "dbY HMS")

read_pdt <- function(pdt_csv) {
  pdt <- readr::read_csv(pdt_csv,
                         progress = FALSE,
                         show_col_types = FALSE) %>% 
    dplyr::select(DeployID, Date, starts_with(c("Depth","MinTemp","MaxTemp"))) %>% 
    dplyr::select(-ends_with("Error"), -DepthSensor) %>% 
    tidyr::pivot_longer(starts_with(c("Depth","MinTemp","MaxTemp")),
                        names_to = ".value",
                        names_pattern = "(^.{0,5})",
                        values_drop_na = TRUE) %>% 
    dplyr::arrange(DeployID,Date,Depth) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(date = lubridate::parse_date_time(date, .orders))
}
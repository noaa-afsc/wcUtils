library(fs)
library(purrr)

locs_df <- dir_ls(path = here::here('inst'), 
                  recurse = TRUE,
                  glob = "*-Locations.csv") %>% 
  map_df(read_locs, .id = "file")

histos_df <- dir_ls(path = here::here('inst'),
                    recurse = TRUE,
                    glob = "*-Histos.csv") %>% 
  map(read_histos)

test_tad <- tidyTimeAtDepth(histos_df[[6]])
test_percent <- tidyTimelines(histos_df[[6]])
test_divedepth <- tidyDiveDepths(histos_df[[6]])
test_diveduration <- tidyDiveDurations(histos_df[[6]])

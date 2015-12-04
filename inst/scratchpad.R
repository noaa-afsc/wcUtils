library(wcUtils)
histos_74627 <- read_histos('inst/extdata/sample_data/74627/74627-Histos.csv')

divedepth_74627 <- tidyDiveDepths(histos_74627)
diveduration_74627 <- tidyDiveDurations(histos_74627)
tad_74267 <- tidyTimeAtDepth(histos_74627)
timelines_74627 <- tidyTimelines(histos_74627)

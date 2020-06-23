test_that("Test tidyTimelines: return tibble of expected dimensions", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  histos <- read_histos(histos_file)
  expect_equal(dim(tidyTimelines(histos)), c(6864,4))
})

test_that("Test tidyTimelines: check names", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  histos <- read_histos(histos_file)
  expect_equal(names(tidyTimelines(histos)), c("deployid", "hist_type", 
                                               "timeline_start_dt", "percent_dry"))
})

test_that("Test tidyTimelines: check for hourly data", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  histos <- read_histos(histos_file)
  t <- tidyTimelines(histos)
  expected_hours <- seq.int(0,23)
  t_hours <- unique(lubridate::hour(t$timeline_start_dt))
  expect_equal(expected_hours, t_hours)
  expected_minutes <- 0
  t_minutes <- unique(lubridate::minute(t$timeline_start_dt))
  expect_equal(expected_minutes, t_minutes)
})

test_that("Test tidyTimelines: percent_dry values from 0-100", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  histos <- read_histos(histos_file)
  t <- tidyTimelines(histos)
  expect_equal(range(t$percent_dry), c(0,100))
})

test_that("Test tidyTimelines: return tibble of expected dimensions for 20min", {
  histos_file <- here::here('tests/testdata/41823-Histos.csv')
  histos <- read_histos(histos_file)
  expect_equal(dim(tidyTimelines(histos)), c(7560,4))
})

test_that("Test tidyTimelines: check for 20min data", {
  histos_file <- here::here('tests/testdata/41823-Histos.csv')
  histos <- read_histos(histos_file)
  t <- tidyTimelines(histos)
  expected_hours <- seq.int(0,23)
  t_hours <- unique(lubridate::hour(t$timeline_start_dt))
  expect_equal(expected_hours, t_hours)
  expected_minutes <- c(0,20,40)
  t_minutes <- unique(lubridate::minute(t$timeline_start_dt))
  expect_equal(expected_minutes, t_minutes)
})
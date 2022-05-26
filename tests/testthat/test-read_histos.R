test_that("Test read_histos: returns list of length 2", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  histos <- read_histos(histos_file)
  expect_equal(names(histos), c("histos","limits"))
})

test_that("Test read_histos: limits for 3 hist_types are found", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  limits <- read_histos(histos_file)$limits
  expect_equal(nrow(limits), 3)
})

test_that("Test read_histos: limit names for 3 hist_types", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  histos <- read_histos(histos_file)
  limit_names <- histos$limits$hist_type
  expect_equal(limit_names, c("TADLIMITS","DiveDepthLIMITS", "DiveDurationLIMITS"))
})

test_that("Test read_histos: correct column names and data types", {
  histos_file <- here::here('tests/testdata/39489-Histos.csv')
  histos <- read_histos(histos_file)
  histos_tbl <- histos$histos
  col_names <- names(histos_tbl)
  expect_equal(head(col_names), c("deployid", "ptt", "depth_sensor", "source",
                                  "instr", "hist_type"))
  expect_equal(tail(col_names), c("bin67","bin68","bin69","bin70",
                                  "bin71","bin72"))
  expected_types <- list(character = 7, integer = 3, numeric = 76, 
                         POSIXct = 1, POSIXt = 1)
  expect_equal(as.list(table(unlist(lapply(histos_tbl, class)))),
                   expected_types)
})

test_that("Test read_histos: older ST16 data", {
  histos_file <- here::here('tests/testdata/41823-Histos.csv')
  histos <- read_histos(histos_file)
  limits <- read_histos(histos_file)$limits
  histos_tbl <- histos$histos
  col_names <- names(histos_tbl)
  expect_equal(names(histos), c("histos","limits"))
  expect_equal(nrow(limits), 0)
  expect_equal(head(col_names), c("deployid", "ptt", "depth_sensor", "source",
                                  "instr", "hist_type"))
  expect_equal(tail(col_names), c("bin67","bin68","bin69","bin70",
                                  "bin71","bin72"))
  expected_types <- list(character = 7, integer = 3, numeric = 76, 
                         POSIXct = 1, POSIXt = 1)
  expect_equal(as.list(table(unlist(lapply(histos_tbl, class)))),
               expected_types)
})
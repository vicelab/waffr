context("Regression tests using the R6 object versus original WAFFR outputs")


test_that("object setup works", {
	input_table <- "Q:/VICE Lab/RESEARCH/PROJECTS/wfar/input/tables/cup+_kc_cdl.csv"
  waffr <- WAFFR$new()
  waffr$kc_input_table <- input_table
  waffr$get_kc_lookups()

  expected_result <- readRDS("reference_outputs/CDL_Kc_LUT_daily_local.rds")

  expect_equal(waffr$kc_lookup_table_daily, expected_result)
})

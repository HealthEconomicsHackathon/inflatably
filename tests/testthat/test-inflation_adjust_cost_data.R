context("test-inflation_adjust_cost_data")

test_that("simple calcs", {
  expect_equivalent(inflation_adjust_cost_data(2014, 2015, 1, "HCHS"), 2.7)
  # expect_equivalent(inflation_adjust_cost_data(2014, 2015, 1, "CPI"), )
  
})

test_that("errors", {
})

inflation_df <- data.frame("year" = 2008:2010,
                           "rate" = c(0.02, 0.03, 0.025))

test_that("asserts custom", {
  expect_error(inflation_adjust_cost_custom(2008.5, 2010, 1, inflation_df),
               regexp = "From date must be an integer valued whole year")
  
  expect_error(inflation_adjust_cost_custom(2008, 2009.5, 1, inflation_df),
               regexp = "To date must be an integer valued whole year")
  
  expect_error(inflation_adjust_cost_custom(2008, 2010, -1, inflation_df),
               regexp = "Cost must be non-negative")
  
  expect_error(inflation_adjust_cost_custom(1008, 2010, 1, inflation_df),
               regexp = "from year not in look-up table")
  
  expect_error(inflation_adjust_cost_custom(2008, 2020, 1, inflation_df),
               regexp = "to year not in look-up table")
})

test_that("asserts main", {
  
  expect_error(inflation_adjust_cost_data(2012, 2015, 2, "abc"),
               "inflation_data name not available")
})
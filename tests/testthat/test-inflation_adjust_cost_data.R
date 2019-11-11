context("test-inflation_adjust_cost_data")

test_that("simple calcs", {
  expect_equivalent(inflation_adjust_cost_data(2014, 2015, 1, "HCHS"), 2.7)
  
})

test_that("errors", {
})
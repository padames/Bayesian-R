library("testthat")
library("here")

source(here::here("scripts", "process_input.R"))

test_that("parse_population processes population data correctly", {
  expected_output <- list(
    random_variate_fun = function(size, params) {
      rnorm(size, mean = params$mean, sd = params$sd)
    },
    size = as.integer(6),
    params = list(
      mean = 0,
      sd = 1
    )
  )
  
  a_population <- list(
    type = "normal",
    mean = 0,
    sd = 1,
    size = 6
  )
  
  calc_output <- parse_population( a_population )

  expect_equal(length(expected_output), length(calc_output))
  expect_equal(expected_output$params, calc_output$params)
  expect_equal(length(expected_output$random_variate_fun), length(calc_output$random_variate_fun))
  expect_equal(expected_output$size, calc_output$size)
  # internals of the function are slightly different
})
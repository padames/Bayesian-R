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
  browser()
  expect_equal(length(expected_output), length(calc_output))
  expect_equal(expected_output$params, calc_output$params)
  expect_equal(length(expected_output$random_variate_fun), length(calc_output$random_variate_fun))
  expect_equal(expected_output$size, calc_output$size)
  # internals of the function are slightly different
})


testthat::test_that("Both populations normally distributed, equal, centered and normalized", {
  expect <- list(list(x=matrix(data = c(-0.8371717, -0.4906859, -0.6937202, 2.4158352, -0.4405479, -1.4482049,  0.1340882,  0.4595894,  0.5747557), nrow = 3,byrow = T),
                      y=matrix(data = c(-1.0236557,  1.1022975, -0.5012581, -0.0151383, -0.4755931, -1.6290935, -0.9359486, -0.7094400, -1.1676193), nrow = 3, byrow = T)))
  calc <- process_input(here("data","test","input_2_std_normal.yaml"), num.simulations = 3, seed = 1234)
  
  expect_equal(expect, calc, tolerance = 1e-5)
})

testthat::test_that("Both populations normally distributed, one standard, the other centered and sd=10", {
  expect <- list(list(x=matrix(data = c(-0.8371717, -0.4906859, -0.6937202,  2.4158352, -0.4405479, -1.4482049,  0.1340882,  0.4595894,  0.5747557), nrow = 3, byrow = T),
                      y=matrix(data = c(-9.236557,  12.022975,  -4.012581,   0.848617,  -3.755931, -15.290935,  -8.359486,  -6.094400, -10.676193), nrow = 3, byrow = T)))
    
  calc <- process_input(here("data","test","input_2_normal_diff_sd.yaml"), 3, 1234)
  
  expect_equal(expect, calc, tolerance = 1e-5)
})

testthat::test_that("Both populations t-student distributed and 4 degrees of freedom", {
  expect <- list(list(x=matrix(data = c(0.3874351, -0.6030411, -0.7634033, -0.6184740,  1.1697739, -2.0625012,  0.1777535,  0.1484711,  0.7600447), nrow = 3, byrow = T),
                      y=matrix(data = c(0.2241069,  1.8902271,  0.6743766,  0.1737002,  0.8496949, -0.1479664,  1.4497662,  0.3998448, -0.5224122),  nrow = 3, byrow = T)))
  calc <- process_input(here("data","test","input_2_t_4df.yaml"), 3, 1234)

  expect_equal(expect, calc, tolerance = 1e-5)
})

testthat::test_that("Both populations exponentially distributed with rate 4", {
  expect <- list(list(x=matrix(data = c(0.4587660068, 0.1820966138, 0.0609095991, 0.1298353177, 0.0958854046, 0.2530569709, 0.4990696655, 0.2351111022, 0.0009987365), nrow = 3, byrow = T),
                      y=matrix(data = c(0.088547264, 0.108635872, 0.001966744, 0.186071874, 0.002272956, 0.203551303, 0.232973397, 0.402571508, 0.007253313), nrow = 3, byrow = T)))
  calc <- process_input(here("data","test","input_2_exp_rate_4.yaml"), 3, 1234)

  expect_equal(expect, calc, tolerance = 1e-5)
})

testthat::test_that("One population normally distributed the other exponential and rate 0.1", {
  
  expect <- list(list(x=matrix(data = c(11.410487, 10.751271,  9.924739,  8.705962, 10.620524, 11.447952, 11.736362, 10.010014,  9.006522), nrow = 3, byrow = T),
                      y=matrix(data = c(0.09091824,  8.14205214,  9.25526209, 16.10286033,  0.29013252,  0.17095131,  0.07866976, 43.24563092, 34.25280541),  nrow = 3, byrow = T)))
  calc <- process_input(here("data","test","input_normal_exp.yaml"), 3, 1234)               

  expect_equal(expect, calc, tolerance = 1e-5)
})
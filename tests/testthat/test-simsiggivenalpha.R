library("testthat")
library("here")

source(here::here("scripts","sim_sig_given_alpha.R"))

test_that("script processes three arguments correctly", {
  alpha <- 0.1
  x.sample.size <- 10
  y.sample.size <- 10
  expect_equal(1027/10000, run_simulations(alpha,x.sample.size, y.sample.size))
})

test_that("script fails when two arguments are provided", {
  alpha <- 0.1
  x.sample.size <- 10
  expect_error(run_simulations(alpha,x.sample.size))
})


test_that("script fails when one argument is provided", {
  alpha <- 0.1
  expect_error(run_simulations(alpha))
})

test_that("script fails when no arguments are provided", {
  expect_error(run_simulations())
})

test_that("generate a vector of given lenght of randon normal standard values", {
  vec.length <- 10
  set.seed(1234)
  
  expected <- rnorm(vec.length)
  
  expect_equal(length(expected), length(gen_normal_standard_random_vector(vec.length)))
  
})

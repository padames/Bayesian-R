library("testthat")
library("here")

source(here::here("scripts","sim_sig_given_alpha.R"))

test_that("script processes three arguments correctly", {
  alpha <- 0.1
  x.sample.size <- 10
  y.sample.size <- 10
  
  expect_equal(1027/10000, run_simulations(alpha,x.sample.size, y.sample.size))
})
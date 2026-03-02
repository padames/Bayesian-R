library("testthat")
library("here")

source(here::here("R","tstatistic.R"))
#the following only works when running testthat
# source("../../R/tstatistic.R")

test_that("t-statistic properly computed",
          {
            x <- c(34,2,-6,30,92,-46,10,31,56)
            y <- c(36,-29, 45,29,67,79,-26,0,10,11)
            m <- length(x)
            n <- length(y)
            sp <- sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
            calc <- (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
            
            expect_equal(calc, tstatistics(x,y))
            
          })
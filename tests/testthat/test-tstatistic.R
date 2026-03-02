library("testthat")
library("here")

source(here::here("R","tstatistic.R"))
#the following only works when running testthat
# source("../../R/tstatistic.R")

test_that("t-statistics properly computed",
          {
            x <- c(34,2,-6,30,92,-46,10,31,56)
            y <- c(36,-29, 45,29,67,79,-26,0,10,11)
            m <- length(x)
            n <- length(y)
            sp <- sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
            calc <- (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
            
            expect_equal(calc, tstatistics(x,y))
            
          })


test_that("t-statistics return vector with NAs for any zero-length sample",
          {
            x <- c()
            y <- c(1,2,3,4,5)

            expect_true(any(is.na(tstatistics(x,y))))
            expect_equal(c(NA,length(y)), tstatistics(x,y))
            expect_equal(c(length(y),NA), tstatistics(y,x))
          })
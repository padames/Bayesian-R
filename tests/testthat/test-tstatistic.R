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
            
            expect_equal(calc, tstatistic(x,y))
            
          })


test_that("t-statistics return vector with NAs for any non-numeric character",
          {
            x <- c("4,5","2","4")
            y <- c("1","2","3","4","5")

            expect_true(any(is.na(tstatistic(x,y))))
            expect_true(any(is.na(tstatistic(y,x))))

          })

test_that("t-statistics raises error for any zero-length sample",
          {
            x <- c()
            y <- c(1,2,3,4,5)
            
            expect_true(any(is.na(tstatistic(x,y))))
            expect_true(any(is.na(tstatistic(y,x))))
          })
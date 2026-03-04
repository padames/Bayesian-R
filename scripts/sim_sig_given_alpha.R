#!/usr/bin/env Rscript

library("here")
library("purrr")

# SCRIPT NAME:      sim_sig_given_alpha
# ARGUMENTS:        N, the number of simulations to run
#                   x_size, the number of elements in the first sample
#                   y_size, the number of elements in the second sample
# DESCRIPTION:      This script computes the significance level of the
#                   probability that the statistic T is greater than 
#                   the percentile of the corresponding t-distribution
#                   with x_size+y_size-2 degrees of freedom.
#                   It does this by running N simulations of the value of
#                   the T statistic for each of the samples and comparing it
#                   to the cumulative probability for the percentile given
#                   through alpha (computed as qt(1-alpha/2, x_size+y_size-2))


run_simulations <- function(alpha, x.sample.size, y.sample.size){
  set.seed(26763)
  source(here::here("R", "tstatistic.R"))
  
  num.simulations <- 10000
  percentile <- 1-alpha/2 # if alpha=0.1 (significance level), then 0.95 (confidence level) for a two-tail confidence interval
  
  # This is a constant value, the t-quartile
  degrees.of.freedom <- x.sample.size+y.sample.size-2
  one.qt <- qt(percentile, degrees.of.freedom)
  qt <- rep(one.qt, num.simulations)
  
  x <- replicate(num.simulations, rnorm(x.sample.size, mean=0, sd=1), simplify=FALSE)
  y <- replicate(num.simulations, rnorm(y.sample.size, mean=0, sd=1), simplify=FALSE)
  
  t <- mapply(tstatistic, as.list(x), as.list(y))
  
  reject.criteria <- abs(t) > qt
  
  num.rejected <- sum(reject.criteria)
  
  return( num.rejected/num.simulations)
  
}

# If the script is executed directly
if (interactive() == FALSE) {
  args <- commandArgs(trailingOnly = TRUE )
  alpha <- as.numeric(args[1])

  x.sample.size <- as.integer(args[2])
  y.sample.size <- as.integer(args[3])
  
  true.significance <- run_simulations(alpha, x.sample.size, y.sample.size)
  print(true.significance)
}
#!/usr/bin/env Rscript


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
suppressPackageStartupMessages({
  library("purrr")
  library("here")
})

source(here::here("R", "tstatistic.R"))
source(here::here("scripts","process_input.R"))

# useful for run_simulations ---legacy
gen_normal_random_vector <- function(len, mean=0, sd=1) {
  rnorm(len, mean=mean, sd=sd)
}

# useful for run_simulations ---legacy
gen_samples <- function(num.simulations, fun_x, fun_y) {
  x <- replicate( num.simulations, fun_x$f( fun_x$size ), simplify=FALSE )
  y <- replicate( num.simulations, fun_y$f( fun_y$size ), simplify=FALSE )
  list(x=x,y=y)
}

# legacy, hard coded testing
run_simulations <- function(alpha, x.sample.size, y.sample.size){
  set.seed(26763)

  num.simulations <- 10000
  percentile <- 1-alpha/2 # if alpha=0.1 (significance level), then 0.95 (confidence level) for a two-tail confidence interval
  
  # This is a constant value, the t-quartile
  degrees.of.freedom <- x.sample.size+y.sample.size-2
  one.qt <- qt(percentile, degrees.of.freedom)
  qt <- rep(one.qt, num.simulations)
  
  f1 <- list( f=gen_normal_random_vector, size=x.sample.size )
  f2 <- list( f=gen_normal_random_vector, size=y.sample.size )
  
  res <- gen_samples(num.simulations = num.simulations, f1, f2)
  
  x <- res$x
  y <- res$y

  t <- mapply(tstatistic, as.list(x), as.list(y))
  
  reject.criteria <- abs(t) > qt
  
  num.rejected <- sum(reject.criteria)
  
  return( num.rejected/num.simulations)
}

compute_one_test <- function(sims, alpha){

  # These are constants at this point (sims already have all Monte Carlo Simulations in it)
  x.sample.size <- nrow(sims$x)
  y.sample.size <- nrow(sims$y)
  num.simulations <- ncol(sims$x)
  
  degrees.of.freedom <- x.sample.size + y.sample.size - 2
  percentile.confidence <- 1 - alpha / 2 # if alpha=0.1 (significance level), then 0.95 (confidence level) for a two-tail confidence interval
  # The quartile is repeated to facilitate the vectorized calculation below
  one.qt <- qt(percentile.confidence, degrees.of.freedom)
  qt <- rep(one.qt, num.simulations)
  
  x <- lapply(seq_len(ncol(sims$x)), function(i) sims$x[, i])
  y <- lapply(seq_len(ncol(sims$y)), function(i) sims$y[, i])
  
  t <- mapply(tstatistic, x, y)
  
  reject.criteria <- abs(t) > qt
  
  num.rejected <- sum(reject.criteria)
  
  return( num.rejected/num.simulations)
}

run_multiple_sims <- function(file.name, num.sim, alpha, seed=NULL) {
  if (! is.null(seed)) {
    set.seed(seed)
  }
  # browser()
  print(file.name)
  print(alpha)
  print(num.sim)
  parsed_runs <- process_input(file.name, num.simulations = num.sim, seed = seed)
  
  sig_levels <- purrr::map(parsed_runs, compute_one_test, alpha)
  
  return(sig_levels)
} 


# If the script is executed directly
if (interactive() == FALSE) {
  
  args <- commandArgs(trailingOnly = TRUE )
  stopifnot(exprs = length(args) >= 1)
  
  alpha <- as.numeric(args[1])

  cat(paste0("All significance tests will be done against a significance of ", alpha, "\n"))  
  
  file_name <- here("data","input", "input_data_multi_run.yaml")
  
  # print(paste0("File name in main script ", file_name))
  sig_levels <- run_multiple_sims(file.name = file_name,
                                  num.sim = 1000L,
                                  alpha = alpha)#,
                                  # seed=1234)
  print(sig_levels)
}
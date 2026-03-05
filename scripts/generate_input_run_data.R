#!/usr/bin/env Rscript

# SCRIPT NAME:      generate_input_run_data
# ARGUMENTS:        None
# DESCRIPTION:      This script generates the input data for simulation runs
#                   of a function that calculates significance levels of
#                   a t-test computed by Monte Carlo experiments.
#                   The inout data created is in the YAML format



run.data <- list(
  list(run_number = 1,
       population_one=c(type = "standard normal",
                        size = 10),
       population_two=c(type = "standard normal",
                        size = 10)),
  list(run_number = 2,
       population_one=c(type = "standard normal",
                        size = 10),
       population_two=c(type = "normal",
                        mean = 1,
                        sd = 10,
                        size = 10)),
  list(run_number = 3,
       population_one=c(type = "t-student",
                        df = 4,
                        size = 10),
       population_two=c(type = "t-student",
                        df = 4,
                        size = 10)),
  list(run_number = 4,
       population_one=c(type = "exponential",
                        rate = 4,
                        size = 10),
       population_two=c(type = "exponential",
                        rate = 4,
                        size = 10)),
  list(run_number = 5,
       population_one=c(type = "normal",
                        mean = 10,
                        sd = 2,
                        size = 10),
       population_two=c(type = "exponential",
                        rate=1/10,
                        size = 10))
) 


yaml::write_yaml(run.data, here("data","input", "input_data_multi_run.yaml"), indent=4)

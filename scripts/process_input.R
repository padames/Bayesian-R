#!/usr/bin/env Rscript

# SCRIPT NAME:      process_input
# ARGUMENTS:        file_name.yaml, the yaml text file with the run data
# DESCRIPTION:      This script takes an input file name as argument to generate 
#                   the R objects necessary to run all the simulations.
#                   The input file is text formatted in YAML
suppressPackageStartupMessages({
    library("yaml")
    library("purrr")
    library("here")
})

parse_population <- function(population_entry) {
    pop_type <- population_entry[[1]]
    pop_args <- as.numeric(population_entry[-1]) # all other entries

    if (any(is.na(pop_args))) {
        stop("Population parameters must be numeric after the type entry.")
    }

    switch(
        pop_type,
        "standard normal" = list(
            random_variate_fun = function(size, params) rnorm(size),
            size = as.integer(pop_args[[1]]),
            params = list()
        ),
        "normal" = list(
            random_variate_fun = function(size, params) {
                rnorm(size, mean = params$mean, sd = params$sd)
            },
            size = as.integer(pop_args[[3]]),
            params = list(
                mean = pop_args[[1]],
                sd = pop_args[[2]]
            )
        ),
        "t-student" = list(
            random_variate_fun = function(size, params) rt(size, df = params$df),
            size = as.integer(pop_args[[2]]),
            params = list(df = pop_args[[1]])
        ),
        "exponential" = list(
            random_variate_fun = function(size, params) rexp(size, rate = params$rate),
            size = as.integer(pop_args[[2]]),
            params = list(rate = pop_args[[1]])
        ),
        stop(paste("Unsupported population type:", pop_type))
    )
}

parse_run <- function(a_run, num.simulations) {
    pop_one <- parse_population(a_run$population_one)
    pop_two <- parse_population(a_run$population_two)

    random_variate_fun_1 <- pop_one$random_variate_fun
    random_variate_fun_2 <- pop_two$random_variate_fun

    x <- replicate(
        num.simulations,
        random_variate_fun_1(pop_one$size, pop_one$params),
        simplify = FALSE # this keeps the output as a list of vectors
    )
    y <- replicate(
        num.simulations,
        random_variate_fun_2(pop_two$size, pop_two$params),
        simplify = FALSE
    )

    list(
        x = do.call(cbind, x),
        y = do.call(cbind, y) # this keeps consistent matrix structure: length of samples x num simulations even for edge cases lkike sample size 1
    )
}

process_input <- function(file_name, num.simulations = 10000) {
    run_data <- yaml::read_yaml(file_name)
    run_ids <- purrr::map_chr(run_data, function(run) as.character(run$run_number))
    parsed_runs <- purrr::map(run_data, parse_run, num.simulations = num.simulations)

    stats::setNames(parsed_runs, run_ids)
    purrr::map(run_data, parse_run, num.simulations = num.simulations)
}

# If the script is executed directly
if (interactive() == FALSE) {
    args <- commandArgs(trailingOnly = TRUE)

    input_file <- if (length(args) >= 1) {
      here::here("data", "input", args[[1]])
    } else {
        here::here("data", "input", "input_data_multi_run.yaml")
    }

    num.simulations <- if (length(args) >= 2) {
        as.integer(args[[2]])
    } else {
        10000L
    }

    parsed_runs <- process_input(input_file, num.simulations = num.simulations)

    run_dimensions <- purrr::imap(parsed_runs, function(run, run_id) {
        list(
            run_number = as.numeric(run_id),
            x_dim = dim(run$x),
            y_dim = dim(run$y)
        )
    })

    print(run_dimensions)
}



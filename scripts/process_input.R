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


# Name:              parse_population
# Input:             A list with the population type and its parameters   
# Output:            Generates the parametric functional forms and the parameter values
#                    necessary to compute random samples from those functions.
#                    This is the function where new types of populations would be added 
parse_population <- function(population_entry) {
    pop_type <- population_entry[[1]]
    pop_args <- as.numeric(population_entry[-1]) # all other entries

    if (length(pop_type) == 0) {
      stop("The input should have a population type, none was defined!")  
    }
    
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

# Name:              parse_a_run
# Input:             The parametric specification to compute two population samples
#                    The number of simulations
#                    A seed if the function is called at the top level, as in unit testing.
# Output:            The population matrices consisting of pseudo-random samples from each population.
#                    More specifically, the output is a list of two matrices. 
#                    Each matrix has as many rows as the size of the population sample
#                    and as many columns as the number of Monte Carlo simulations requested.
parse_a_run <- function(populations.spec.for.this.run, num.simulations, seed = NULL) {
    if( ! is.null(seed) ) {
      set.seed(seed)
    }
    
    pop_one <- parse_population(populations.spec.for.this.run$population_one)
    pop_two <- parse_population(populations.spec.for.this.run$population_two)

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

    values.in.this.run.are.matrices.of.pop.size.by.num.sim <- list(
        x = do.call(cbind, x),
        y = do.call(cbind, y) # this keeps consistent matrix structure: length of samples x num simulations even for edge cases like sample size 1
    )
    # to generate test stubs:
    # save(populations.spec.for.this.run, values.in.this.run.are.matrices.of.pop.size.by.num.sim, file = here("data","test","run_data"))
    
    return(values.in.this.run.are.matrices.of.pop.size.by.num.sim)
}

# Name:              parse_a_run2
# Input:             The parametric specification to compute two population samples
#                    The number of simulations
#                    A seed if the function is called at the top level, as in unit testing.
# Output:            A list with the name of the models used and 
#                    the population matrices consisting of pseudo-random samples from each population.
#                    These matrices are separate elements of the list, and 
#                    more specifically, each matrix has as many rows as the size of the population sample
#                    and as many columns as the number of Monte Carlo simulations requested.
parse_a_run2 <- function(populations.spec.for.this.run, num.simulations, seed = NULL) {
  if( ! is.null(seed) ) {
    set.seed(seed)
  }
  
  pop_one <- parse_population(populations.spec.for.this.run$population_one)
  pop_two <- parse_population(populations.spec.for.this.run$population_two)
  
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
  
  values.in.this.run.are.matrices.of.pop.size.by.num.sim <- list(
    x = do.call(cbind, x),
    y = do.call(cbind, y) # this keeps consistent matrix structure: length of samples x num simulations even for edge cases like sample size 1
  )
  # to generate test stubs:
  # save(populations.spec.for.this.run, values.in.this.run.are.matrices.of.pop.size.by.num.sim, file = here("data","test","run_data"))
  
  return(values.in.this.run.are.matrices.of.pop.size.by.num.sim)
}


# Name:              process_input
# Input:             1. A valid file name stored in the file system or url
#                    2. The number of simulations to generate population values.
#                    3. The seed for the pseudo-random simulations. Required for reproducibility.
# Output:            Generates the parametric functional forms and the parameter values
#                    necessary to compute random samples from those functions.
#                    This is the function where new types of populations would be added 
process_input <- function(file_name, num.simulations = 10000, seed = NULL) {
    if( ! is.null(seed) ) {
      set.seed(seed)
    }

    all_runs <- yaml::read_yaml(file_name)
    run_ids <- purrr::map_chr(all_runs, function(one_run) as.character(one_run$run_number))
    
    parsed_runs <- purrr::map(all_runs, parse_a_run, num.simulations = num.simulations)
    
    stats::setNames(parsed_runs, run_ids)
    parsed_runs <- purrr::map(all_runs, parse_a_run, num.simulations = num.simulations)
    return(parsed_runs)
}


# Name:              model_to_string
# Input:             1. A list describing a single population model for simulation
# Output:            the statistical model plus parameter values used to simulate a 
#                    population sample
model_to_string <- function(model) {
  # "type" first, then all remaining args as key=value
  args <- model[setdiff(names(model), "type")]
  paste(
    c(
      model$type,
      sprintf("%s=%s", names(args), unlist(args, use.names = FALSE))
    ),
    collapse = " "
  )
}


# Name:              run_to_string
# Input:             1. A list of a case with two population models for simulation
# Output:            the compound name of the two statistical models plus their parameter
#                    values used to simulate a case when comparing two population samples
# Notes:             The input list has three elements defined by:
#                    list(run_number=1,
#                         population_1=list(type="model1",param1=val1,param2=val2),
#                         population_2=list(type="model2",param1=val1,param2=val2))
#                    This function is robust if one or both populations are missing
run_to_string <- function(run) {
  
  name.filter.for.the.two.populations <- intersect(c("population_one", "population_two"), names(run))
  
  pops <- run[name.filter.for.the.two.populations]

  # print(paste("run$run_number=", run$run_number))
  
  if (length(pops) == 0L) return("")
  
  the_label <- paste(vapply(pops, model_to_string, character(1)), collapse = "; ")
  
  return(paste0("(",run$run_number,") ",the_label))
}


# Name:              process_input2
# Input:             1. A valid file name stored in the file system or url
#                    2. The number of simulations to generate population values.
#                    3. The seed for the pseudo-random simulations. Required for reproducibility.
# Output:            Generates the parametric functional forms and the parameter values
#                    necessary to compute random samples from those functions.
#                    This is the function where new types of populations would be added 
process_input2 <- function(file_name, num.simulations = 10000, seed = NULL) {
    if( ! is.null(seed) ) {
      set.seed(seed)
    }
    
    all_runs <- yaml::read_yaml(file_name)
    # print("all_runs=")
    # print(all_runs)
    
    labels <- vapply(all_runs, run_to_string, character(1)) 

    # print( paste0("labels=", labels, "" ))
    
    run_ids <- purrr::map_chr(all_runs, function(one_run) as.character(one_run$run_number))
    
    parsed_runs <- purrr::map(all_runs, parse_a_run2, num.simulations = num.simulations)
    
    stats::setNames(parsed_runs, run_ids)
    
    return(list(labels=labels,runs=parsed_runs))
}


process_input3 <- function(file_name, num.simulations = 10000, seed = NULL, num.workers = 12L) {
  if (!is.null(seed)) {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
  }
  
  all_runs <- yaml::read_yaml(file_name)
  labels <- vapply(all_runs, run_to_string, character(1))
  run_ids <- purrr::map_chr(all_runs, ~ as.character(.x$run_number))
  
  parsed_runs <- parallel::mclapply(
    all_runs,
    parse_a_run2,
    num.simulations = num.simulations,
    mc.cores = num.workers,
    mc.set.seed = TRUE
  )
  
  parsed_runs <- stats::setNames(parsed_runs, run_ids)
  list(labels = labels, runs = parsed_runs)
}


# If the script is executed directly (not sourced by another script)
if (interactive() == FALSE &&
    sys.nframe() == 0L) { # this one guarantees no calling function
    args <- commandArgs(trailingOnly = TRUE)

    input_file <- if (length(args) >= 1) {
      if (grepl("^https?://", args[[1]]) || file.exists(args[[1]])) {
        args[[1]]
      } else {
        here::here("data", "input", args[[1]])
      }
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



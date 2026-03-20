# Exercises and Examples From the Bayesian Computation with R, by Jim Albert, Springer, 2007.
I found this wonderful book in the Use R! series. 
This repo contains refactored and expanded R code from the examples in the book.
I have added unit tests and proper project organization with the intention of guiding students and readers towards disciplined software engineering skills when writing R simulation code.

## After cloning

To run scripts and tests reliably recover the environment by typing the following at the R console:

```r
renv::restore()
```

This gets you a consistent environment to run the R code in this project without colliding with any other versions of libraries used by other projects or installed in your computer at the user level.


## Chapter  One: An  Introduction to R
### Exploring the Robustness of the t Statistic

The T statistic is used in sample theory to make inferences about the difference in the mean of two populations when using small independent samples.
The assumptions about the populations are that they are normally distributed and that their variance are equal.

If we wish to validate the following null hypothesis: 

$$
  H_0: \mu_x = \mu_y
$$

We can draw two small random samples: ${x_1, x_2, \ldots, x_n}$ of size $n$, and ${y_1, y_2, \ldots, y_m}$ of size $m$,
sample means $\bar{X}$ and $\bar{Y}$, and sample standard deviations $s_x$ and $s_y$, from populations with means $\mu_x$ and $\mu_y$, and standard deviations $\sigma_x=\sigma_y$.

Then the T statistic can model the difference between the population means:

$$
  T = \frac{(\bar{X} - \bar{Y}) - (\mu_x - \mu_y)}{S\sqrt{\frac{1}{n}+\frac{1}{m}}}
$$

where $S$ is a pooled sample distribution standard deviation, given by:

$$
  S = \sqrt{\frac{(m-1)s_x^2 + (n-1)s_y^2}{m+n-2}}
$$

Under the Null Hypothesis, the test statistic $T$ has a t-student distribution with $(m+n-2)$ degrees of freedom.

A confidence interval can be built with:

$$
  (\bar{X} - \bar{Y}) \pm t_{\alpha/2}S\sqrt{\frac{1}{n}+\frac{1}{m}}
$$

$t_{\alpha/2}$ comes from the single-side t cumulative probability distribution at $1-\alpha$ confidence.  
Thus, one would reject $H_0$ if $|T|>t_{m+n-2, \alpha/2}$.

### Estimates of the true significance, $\alpha$

In order to explore how robust is the T statistic when the populations are less 
than idel for the model assumptions, we can simulate samples taken from normal 
populations that have different standard deviations and from populations that 
are not normally distributed.
We could also explore the sensitivity to sample size and population mean for 
normally distributed populations.
In each case the value we can compute, an estimate of the true significance, 
$\alpha^T$ can be estimated via:

$$
  \alpha^T = P(|T|>t_{n+m-2,\alpha/2})
$$

We can estimate this probability numerically by counting how many samples satisfy 
the inequality and dividing by the total number of samples, $N$:

$$
  \hat{\alpha}^T = \frac{\text{Number of rejections of}H_0}{N}
$$

I took the liberty of refactoring the original code to avoid the for loop, preferring a vectorized form with a more functional flavour.
To be concrete, I changed the original version:

```r
alpha=0.1; m=10; n=10                 # sets the significance level, and population sizes m and n
N= 10000                              # sets the number of simulations
n.reject=0                            # counter of number of rejections
for (i in 1..N) {
  # compute the population sample vectors called x and y from statistical distributions
  x = rnorm(m,mean=0,sd=1)
  y = rnorm(n,mean=0,sd=1)
  t = tstatistic(x,y)                 # the t statistic of the two populations
  if (abs(t) > qt(1-alpha)/2, n+m-2)
    n.reject = n.reject + 1           # reject if |t| exceeds critical probability of t (pt)
}
true.sig.level = n.reject / N
```
For a version that would look more like the following:

```r
run_simulations <- function(alpha, x.sample.size, y.sample.size){
  num.simulations <- 10000
  degrees.of.freedom <- x.sample.size + y.sample.size - 2
  one.qt <- qt(1-alpha/2, degrees.of.freedom)
  # for a functional style of formulation, create a vector of length num.simulations with this value 
  qt <- rep(one.qt, num.simulations)
  # the following are lists of vectors, one each from each population sample drawn from the distribution
  x <- replicate( num.simulations, rnorm(x.sample.size, mean=0, sd=1), simplify=FALSE ) 
  y <- replicate( num.simulations, rnorm(y.sample.size, mean=0, sd=1), simplify=FALSE )
  # the following applies the function 'tstatistic' to pairs of vectors from x and y
  mapply( tstatistic, as.list(x), as.list(y))
 
  reject.criteria <- abs(t) > qt
  
  num.rejected <- sum(reject.criteria)
  
  return( num.rejected/num.simulations)
}
```
Although it may seem harder to interpret, the latter form processes all the simulation samples in one pass.
The iteration is contained in the `mapply` function call. The function receives two lists of vectors, below for a case  of 3 simulations and samples of size 4 in each case:

```
> x
[[1]]
[1] -0.6063149  2.0200296 -0.4946306  0.4332016

[[2]]
[1] -1.6955075  0.6542198  0.5501262  1.5884974

[[3]]
[1]  0.6167362  0.7327744  0.1457517 -0.4406999

> y
[[1]]
[1]  2.0084389 -0.7271245  0.5968161 -0.6276610

[[2]]
[1] -0.03036191 -0.26605870 -1.27699706  0.40636897

[[3]]
[1] -0.8204265 -1.2380828 -1.1995675 -0.8659010
```
The function `tstatistic` gets applied to the first vector of x, `x[[1]]` and `y[[1]]` like this by `mapply`:
```r
c(tstatistic(x[[1]], y[[1]]), tstatistic(x[[2]], y[[2]]), tstatistic(x[[3]], y[[3]]))
```
The net effect is the following vector:
```r
[1] 0.02884408 0.72309794 4.49001842
```
Which is then used in the vectorized expression `reject.criteria <- abs(t) > qt`. Both `t` and `qt` are numeric vectors. 

## Project organization

The projects for each section of the chapters are organized as closely as possible to the following:
```
.
├── data
│   ├── input
│   ├── processed
│   ├── raw
│   └── test
├── images  
├── R
├── scripts
└── tests
    └── testthat
```
The root is represented by the `.` in the above diagram. Not shown are folders used by dependency manager packages like `renv`, which lock the versions of R and all project packages to make it easier to transfer to other machines or a container. 
The folder R holds functions sourced individually by scripts. The files in the `scripts` folder can be written in any language, such as Bash, PowerShell, Python, or R, and are used to drive project execution. 

Any script file should ideally be callable as a program from the command line with or without arguments. They should also be sourceable from other files. The latter property may expose internal functions for reuse in other scripts and allows unit testing of the scripts. 


## Unit-testing

Every function created in the `scripts` and `R` folders has corresponding unit tests in the folder `<project_root>/tests/testthat/`.
Unit tests can be run by sourcing the individual file or by sourcing or executing at the command line the script `run_tests.R`:
```r
library(testthat)
testthat::test_dir("tests/testthat/")
```

## Relative Paths

To source files from different paths within the project, the package `here` is used extensively. It is based on the idea that every path descends from the top of the project like the root of a plant. Thus, to source a function in a file called `my_function.R` in the `R` folder and a test input file called `my_data.csv` in the `data` folder from a file in the `tests/testhat` path, one would use: 

```r
source(here("R","my_function.R"))
test_data <- read.csv(here("data","test","my_data.csv"))
```

## Test the master scripts

Chapter One of this book has a table with the results of Montecarlo simulations 
to compute the true significance level at which the null hypothesis of equal
population means is rejected using the T statistic for five different scenarios.
Each scenario consists of drawing samples of a given small size from two 
simulated populations of known statistical distributions.
The scenarios are designed to match the assumptions of the 
T statistic about the populations' distribution and variance to different degrees
of accuracy.
The more radically different the assumptions, we expect the true significance will
be from the theoretical one.

The results show how much the simulated significance level deviates from the 
theoretical one. 

Run the following command at the terminal when the prompt is at the project root:

```bash
 ./scripts/sim_sig_given_alpha.R 0.1 3989

True significance calculations will be done against a theoretical significance of 0.1
The fixed seed 3989, will be used for the pseudo-random calculations
Reading input data from: /home/pablo/git/Bayesian-R/data/input/input_data_multi_run.yaml


|Description                                                   | true_sig|
|:-------------------------------------------------------------|--------:|
|(1) standard normal size=10; standard normal size=10          |    0.099|
|(2) standard normal size=10; normal mean=1 sd=10 size=10      |    0.148|
|(3) t-student df=4 size=10; t-student df=4 size=10            |    0.101|
|(4) exponential rate=4 size=10; exponential rate=4 size=10    |    0.090|
|(5) normal mean=10 sd=2 size=10; exponential rate=0.1 size=10 |    0.146|
```
One great advantage of running code is the flexibility of modifying inputs.
To create different scenarios, one can use a script like `generate_input_run_data.R`
and then feed that to the main script as the third parameter.


## Further thoughts.

The sensitivity of the values to the actual value of the random seed suggests 
the number of simulations can be increased for more stable estimates.
This required more code changes to improve the speed of the code when increasing the
number of Montecarlo simulations from the original 10,000 to 500,000 per case.

The AI suggestions for my Ubuntu 24.04 machine with R 4.5.2 was to use `parallel::mclapply` 
instead of purrr:map in critical sections. Also for reproducible pseudo-random
sampling to use `RNGkind("L'Ecuyer-CMRG")` before setting the seed.

The parallelization changes were reflected in the functions `process_input3` 
and `run_multiple_sims3`.

Now I can run 2,500,000 simulations for the 5 cases suggested in the book in
under 4 minutes user time with any four digit seed to get similar values.

```bash
~/git/Bayesian-R$ time ./scripts/sim_sig_given_alpha.R 0.1 3990 500000

True significance calculations will be done against a theoretical significance of 0.1
The fixed seed 3990, will be used for the pseudo-random calculations
You have set the number of simulations to 500000.
Reading input data from: /home/pablo/git/Bayesian-R/data/input/input_data_multi_run.yaml


|Description                                                   | true_sig|
|:-------------------------------------------------------------|--------:|
|(1) standard normal size=10; standard normal size=10          | 0.100678|
|(2) standard normal size=10; normal mean=1 sd=10 size=10      | 0.131440|
|(3) t-student df=4 size=10; t-student df=4 size=10            | 0.096992|
|(4) exponential rate=4 size=10; exponential rate=4 size=10    | 0.096442|
|(5) normal mean=10 sd=2 size=10; exponential rate=0.1 size=10 | 0.154458|


real    0m49.360s
user    3m41.260s
sys     0m6.307s
```
The calculated values for true significance, $\alpha^T$, were too sensitive
to the specific seed used for the default number of simulations: 10,000.
The value of the first case should be close to the theoretical, 0.1000.
Increasing the number of simulations removed this sensitivity. 
Below, I show an example of a different seed that used to cause oscillations
when using a smaller total number of simulations per case, but with 500,000 is 
stable at the expected theoretical value within three significant digits.

```bash
~/git/Bayesian-R$ time ./scripts/sim_sig_given_alpha.R 0.1 1234 500000

True significance calculations will be done against a theoretical significance of 0.1
The fixed seed 1234, will be used for the pseudo-random calculations
You have set the number of simulations to 500000.
Reading input data from: /home/pablo/git/Bayesian-R/data/input/input_data_multi_run.yaml


|Description                                                   | true_sig|
|:-------------------------------------------------------------|--------:|
|(1) standard normal size=10; standard normal size=10          | 0.100558|
|(2) standard normal size=10; normal mean=1 sd=10 size=10      | 0.132186|
|(3) t-student df=4 size=10; t-student df=4 size=10            | 0.097620|
|(4) exponential rate=4 size=10; exponential rate=4 size=10    | 0.096628|
|(5) normal mean=10 sd=2 size=10; exponential rate=0.1 size=10 | 0.153712|


real    0m49.981s
user    3m42.847s
sys     0m6.372s
```


My laptop CPU specifications are:

```bash
~/git/Bayesian-R$ lscpu
Architecture:             x86_64
  CPU op-mode(s):         32-bit, 64-bit
  Address sizes:          39 bits physical, 48 bits virtual
  Byte Order:             Little Endian
CPU(s):                   12
  On-line CPU(s) list:    0-11
Vendor ID:                GenuineIntel
  Model name:             Intel(R) Core(TM) i7-10710U CPU @ 1.10GHz
    CPU family:           6
    Model:                166
    Thread(s) per core:   2
    Core(s) per socket:   6
    Socket(s):            1
    Stepping:             0
    CPU(s) scaling MHz:   17%
    CPU max MHz:          4700.0000
    CPU min MHz:          400.0000

```

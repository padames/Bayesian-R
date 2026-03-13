# Exercises and Examples From the Book Bayesian Computation with R, by Jim Albert
I found this wonderful book in the Use R! series from Springer. I have compiled expanded R code inspired by the simulation code presented in the book.
I have added unit tests and proper project organization to the code in the hope that this may help students and readers of the book to improve their software engineering skills when writing R code.

## Chapter  One: An  Introduction to R

Exploring the robustness of the t-statistic is an excellent first example of R's powerful expressiveness for numerical simulation and statistical computation.
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

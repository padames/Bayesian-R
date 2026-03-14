# NAME:            tstatistic
# DESCRIPTION:     the T statistic used to test Null hypothesis about 
#                  equal means in two populations. 
# INPUT:           x, a numeric vector of samples from the first population
#                  y, a numeric vector of samples from the second population
# RETURN:          the calculated value of the T statistic
tstatistic=function(x,y) {
  m <- length(x)
  n <- length(y)
  
  m  <- ifelse(m>0, m, NA)
  n  <- ifelse(n>0, n, NA)

  if( any(is.na(c(m,n)))) {
    return (c(m,n))
  }
  
  if( any(!is.numeric(x), !is.numeric(y))) {
    return (c(NA,NA))
  }

  sp <- sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
  (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
}
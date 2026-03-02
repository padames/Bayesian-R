# the st
tstatistics=function(x,y) {
  m  <- length(x)
  n  <- length(y)
  sp <- sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
  (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
}
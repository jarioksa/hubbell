"logn.fun" <-
function(x, lmean, lsd)
{
  x <- log(x)
  n <- length(x)
  r <- seq(along=x)
  sol <- exp(lsd*qnorm(ppoints(n))+lmean)
  sol[order(order(x))]
}

"logn.ml" <-
function(p,x)
{
  -sum(dpois(x, logn.fun(x, p[1], p[2]), log=T))
}

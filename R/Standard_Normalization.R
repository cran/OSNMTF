Standard_Normalization <- function(x) {
  x = as.matrix(x);
  mean = apply(x, 2, mean)
  sd = apply(x, 2, sd)
  sd[sd==0] = 1
  xNorm = t((t(x) - mean) / sd)
  return(xNorm)
}

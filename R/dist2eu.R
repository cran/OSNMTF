dist2eu <- function(X,C) {
  ndata = nrow(X)
  ncentres = nrow(C)

  res = matrix(0,ndata,ncentres)
  for (i in 1:ndata)
  {
    for (j in 1:ncentres)
    {
      res[i,j] = sum((X[i,]-C[j,])^2)
    }
  }
  return(res)
}

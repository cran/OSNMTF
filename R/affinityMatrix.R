affinityMatrix <- function(Diff,K=20,sigma=0.5) {
  N = nrow(Diff)
  sortedColumns = as.matrix(t(apply(Diff,2,sort)))
  finiteMean <- function(x) { mean(x[is.finite(x)]) }
  means1 = apply(sortedColumns[,1:K],1,finiteMean)+.Machine$double.eps;
  sortedRows <- as.matrix(t(apply(Diff,1,sort)))
  means2 = apply(sortedRows[,1:K],1,finiteMean)+.Machine$double.eps;

  avg <- function(x,y) (x+y)
  Sig = outer(means2,means1,avg)/3 + Diff/3 + .Machine$double.eps;
  Sig[Sig <= .Machine$double.eps] = .Machine$double.eps
  densities = dnorm(Diff,0,sigma*Sig,log = FALSE)

  return(densities)
}

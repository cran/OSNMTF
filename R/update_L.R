update_L <- function(W,init_list)
{
  L <- init_list[[1]]
  C <- init_list[[2]]
  R <- init_list[[3]]
  nominator <- W%*%R%*%t(C)
  dominator <- L%*%t(L)%*%W%*%R%*%t(C)
  for (i in 1:nrow(L))
  {
    for (j in 1:ncol(L))
    {
      if (dominator[i,j]>.Machine$double.eps)
      {
        L[i,j] <- L[i,j]*nominator[i,j]/dominator[i,j]
      }
    }
  }
  init_list[[1]] <- L
  return(init_list)
}
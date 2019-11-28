update_R <- function(W,update_B_list)
{
  L <- update_B_list[[1]]
  C <- update_B_list[[2]]
  R <- update_B_list[[3]]
  nominator <- t(W)%*%L%*%C
  dominator <- R%*%t(R)%*%t(W)%*%L%*%C
  for (i in 1:nrow(R))
  {
    for (j in 1:ncol(R))
    {
      if (dominator[i,j]>.Machine$double.eps)
      {
        R[i,j] <- R[i,j]*nominator[i,j]/dominator[i,j]
      }
    }
  }
  update_B_list[[3]] <- R
  return(update_B_list)
}

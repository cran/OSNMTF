update_B <- function(W,update_L_list)
{
  L <- update_L_list[[1]]
  C <- update_L_list[[2]]
  R <- update_L_list[[3]]
  B <- update_L_list[[4]]
  Y <- update_L_list[[5]]
  miu <- update_L_list[[6]]
  nominator <- t(L)%*%W+miu*C%*%t(R)
  dominator <- t(L)%*%L%*%B+Y+miu*B
  for (i in 1:nrow(B))
  {
    for (j in 1:ncol(B))
    {
      if (dominator[i,j]>.Machine$double.eps)
      {
        B[i,j] <- B[i,j]*nominator[i,j]/dominator[i,j]
      }
    }
  }
  update_L_list[[4]] <- B
  return(update_L_list)
}

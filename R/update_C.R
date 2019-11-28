update_C <- function(W, update_R_list, lambda=0.2, rho = 1.1)
{
  L <- update_R_list[[1]]
  C <- update_R_list[[2]]
  R <- update_R_list[[3]]
  B <- update_R_list[[4]]
  Y <- update_R_list[[5]]
  miu <- update_R_list[[6]]
  nominator <- Y%*%R+miu*B%*%R
  dominator <- miu*C%*%t(R)%*%R+lambda
  for (i in 1:nrow(C))
  {
    for (j in 1:ncol(C))
    {
      if (dominator[i,j]>.Machine$double.eps)
      {
        C[i,j] <- C[i,j]*nominator[i,j]/dominator[i,j]
      }
    }
  }
  update_R_list[[2]] <- C
  if (update_R_list[[6]]<10^10)
  {
    new_Y <- Y+miu*(abs(B-C%*%t(R)))
    update_R_list[[5]] <- new_Y
    update_R_list[[6]] <- update_R_list[[6]]*rho
  }
  return(update_R_list)
}

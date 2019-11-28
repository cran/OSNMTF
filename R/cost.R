cost <- function(W,init_list,lambda=0.2)
{
  res <- 0
  L <- init_list[[1]]
  C <- init_list[[2]]
  R <- init_list[[3]]
  matr_loss <- W-L%*%C%*%t(R)
  res <- res+(norm(matr_loss,"F")^2)/2
  res <- res+lambda*sum(init_list[[2]])
  return(res)
}

initialization <- function(W,k,l)
{
  res <- vector('list', 6)
  aa <- kmeans(W,k,1000,20)
  L1 <- matrix(0,nrow(W),k)
  for (i in 1:nrow(W))
  {
    L1[i,aa$cluster[i]] <- 1
  }
  temp_L <- t(L1)%*%L1
  temp_L1 <- solve(temp_L)
  for (i in 1:nrow(temp_L1))
  {
    temp_L1[i,i] <- sqrt(temp_L1[i,i])
  }
  LL <- L1%*%temp_L1
  L1 <- LL+10^-5
  bb <- kmeans(t(W),l,1000,20)
  R1 <- matrix(0,ncol(W),l)
  for (i in 1:ncol(W))
  {
    R1[i,bb$cluster[i]] <- 1
  }
  temp_R <- t(R1)%*%R1
  temp_R1 <- solve(temp_R)
  for (i in 1:nrow(temp_R1))
  {
    temp_R1[i,i] <- sqrt(temp_R1[i,i])
  }
  RR <- R1%*%temp_R1
  R1 <- RR+10^-5
  C1 <- t(L1)%*%W%*%R1
  CR <- C1%*%t(R1)
  random_data <- abs(rnorm(nrow(CR)*ncol(CR),0,0.01))
  Y <- matrix(random_data,nrow(CR),ncol(CR))
  res[[1]] <- L1 #L
  res[[2]] <- C1 #C
  res[[3]] <- R1 #R
  res[[4]] <- CR #B
  res[[5]] <- Y #Y
  res[[6]] <- 0.5/max(C1[C1>0])
  return(res)
}
simu_data_generation <- function(){
  data1 <- matrix(0,100,100)
  data2 <- matrix(0,80,100)
  ### Data1 in the first cluster, with mean 10, sd 1.
  for (i in 1:20)
  {
    data1[i,] <- rnorm(100,10,1)
  }
  ### Data1 in the second cluster, with mean 20, sd 1.
  for (i in 21:40)
  {
    data1[i,] <- rnorm(100,20,1)
  }
  ### Data1 in the third cluster, with mean 30, sd 1.
  for (i in 41:60)
  {
    data1[i,] <- rnorm(100,30,1)
  }
  ### Data1 in the fourth cluster, with mean 40, sd 1.
  for (i in 61:80)
  {
    data1[i,] <- rnorm(100,40,1)
  }
  ### Data1 in the fifth cluster, with mean 50, sd 1.
  for (i in 81:100)
  {
    data1[i,] <- rnorm(100,50,1)
  }
  ### Data2 in the first cluster, with  mean 5, sd 1.
  for (i in 1:20)
  {
    data2[i,] <- rnorm(100,5,1)
  }
  ### Data2 in the second cluster, with mean 10, sd 1.
  for (i in 21:40)
  {
    data2[i,] <- rnorm(100,10,1)
  }
  ### Data2 in the third cluster, with mean 15, sd 1.
  for (i in 41:60)
  {
    data2[i,] <- rnorm(100,15,1)
  }
  ### Data2 in the fourth cluster, with mean 20, sd 1.
  for (i in 61:80)
  {
    data2[i,] <- rnorm(100,20,1)
  }
  new_data1 <- Standard_Normalization(data1)
  new_data2 <- Standard_Normalization(data2)
  dist1 <- dist2eu(new_data1,new_data2)
  simi_matr1 <- affinityMatrix(dist1)
  return(simi_matr1)
}

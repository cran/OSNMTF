OSNMTF<- function(W,lambda=0.2,theta=10^-4,k,l){
  if (!is.matrix(W))
  {
    stop('Error:please provide a data matrix by W')
  }
  if (is.na(k))
  {
    stop('Error:please provide a row cluster number by k')
  }
  if (is.na(l))
  {
    stop('Error:please provide a column cluster number by l')
  }
  init_list <- initialization(W,k,l)  ## initialize the matrix W into L*C*t(R)
  ### minimize the objective function until convergence
  divergence <- 1 ##initialize the divergence
  iter_num <- 1  ##initialize the iteration number
  old_cost <- 0   ##initialize the cost
  last_L <- init_list[[1]]
  ori_C_norm <- norm(init_list[[2]],'F')
  while (divergence>theta)  ##while not convergent
  {
    update_L_list <- update_L(W,init_list)  ##update the sub-matrix L with update_L
    update_B_list <- update_B(W,update_L_list)  ##update the sub-matrix C with update_C
    update_R_list <- update_R(W,update_B_list)  ##update the sub-matrix R with update_R
    update_C_list <- update_C(W,update_R_list,lambda,rho=1.1)  ##update the sub-matrix C with update_C
    folds <- norm(update_C_list[[2]],'F')/ori_C_norm
    update_C_list[[2]] <- update_C_list[[2]]/folds
    init_list <- update_C_list
    new_L <- init_list[[1]]
    new_cost <- norm(new_L-last_L,'F')^2
    divergence <- new_cost/(norm(last_L,'F')^2)
    last_L <- new_L
    old_cost <- new_cost
    iter_num <- iter_num+1
    if (iter_num>1000)
    {
      break
    }
  }
  L_matrix <- init_list[[1]] ## sub_matrix L is the first one in init_list
  L_clu_res <- kmeans(L_matrix,k,1000,20) ## clustering the samples by kmeans on the sub_matrix alpha, which is a conventional method in matrix factorization based clustering.
  L_cluster_result <- L_clu_res$cluster ## get the clustering result, the ith element indicates the cluster of the ith sample
  R_matrix <- init_list[[3]] ## sub_matrix R is the third one in init_list
  R_clu_res <- kmeans(R_matrix,l,1000,20) ## clustering the samples by kmeans on the sub_matrix alpha, which is a conventional method in matrix factorization based clustering.
  R_cluster_result <- R_clu_res$cluster ## get the clustering result, the ith element indicates the cluster of the ith sample
  result <- vector("list",2)
  names(result) <- c("sub_matrices","cluster_results")
  sub_result1 <- init_list
  names(sub_result1) <- c('sub_matrix_L','sub_matrix_C','sub_matrix_R')
  sub_result2 <- vector('list',2)
  sub_result2[[1]] <- L_cluster_result
  sub_result2[[2]] <- R_cluster_result
  names(sub_result2) <- c('L_cluster','R_cluster')
  result[[1]] <- sub_result1
  result[[2]] <- sub_result2
  return(result)
}

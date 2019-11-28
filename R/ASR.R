ASR <- function(row_cluster,col_cluster,W)
{
  row_cluster_num <- length(unique(row_cluster))
  col_cluster_num <- length(unique(col_cluster))
  block_num <- row_cluster_num*col_cluster_num
  temp_values <- vector('numeric',0)
  for (i in 1:row_cluster_num)
  {
    temp_rows <- which(row_cluster==i,TRUE)
    for (j in 1:col_cluster_num)
    {
      temp_cols <- which(col_cluster==j,TRUE)
      temp_values <- c(temp_values,MSR(W[temp_rows,temp_cols]))
    }
  }
  return(sum(temp_values)/block_num)
}

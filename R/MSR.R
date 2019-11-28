MSR <- function(Block)
{
  Block <- as.matrix(Block)
  row_num <- nrow(Block)
  col_num <- ncol(Block)
  block_size <- row_num*col_num
  residue <- Block
  for (i in 1:row_num)
  {
    for (j in 1:col_num)
    {
      residue[i,j] <- Block[i,j]-sum(Block[i,])/col_num-sum(Block[,j])/row_num+Block[i,j]/block_size
    }
  }
  return(sum(residue)/block_size)
}
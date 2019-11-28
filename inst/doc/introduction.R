## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  #Install
#  install.packages('OSNMTF')
#  #Load
#  library(OSNMTF)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages('/path/to/file/OSNMTF.tar.gz',repos=NULL,type="source")

## ----eval=FALSE---------------------------------------------------------------
#  simu_data = simu_data_generation()

## ----eval=FALSE---------------------------------------------------------------
#  # Factorize the matrix with OSNMTF
#  OSNMTF_res <- OSNMTF(simu_data,k=5,l=4)
#  # Get the row coefficient matrix
#  row_coef <- OSNMTF_res[[1]][[1]]
#  # Get the association matrix
#  asso_matrix <- OSNMTF_res[[1]][[2]]
#  # Get the column coefficient matrix
#  column_coef <- OSNMTF_res[[1]][[3]]
#  # Get the row cluster results
#  row_cluster <- OSNMTF_res[[2]][[1]]
#  # Get the column cluster results
#  column_cluster <- OSNMTF_res[[2]][[2]]

## ----eval=FALSE---------------------------------------------------------------
#  # Specify your desired cluster number evaluation interval by your prior knowledge of the data
#  ASR_matrix <- matrix(0,5,5)
#  for (i in 1:5)
#  {
#    rankk <- i+2
#    for (j in 1:5)
#    {
#      rankl <- j+2
#      temp_res <- OSNMTF(simu_data,k=rankk,l=rankl)
#      row_clu1 <- temp_res[[2]][[1]]
#      col_clu1 <- temp_res[[2]][[2]]
#      # MNSR_matrix[i,j] <- MNSR(row_clu1,col_clu1,simi_matr1)
#      ASR_matrix[i,j] <- ASR(row_clu1,col_clu1,simu_data)
#    }
#  }


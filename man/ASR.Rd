% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASR.R
\name{ASR}
\alias{ASR}
\title{Average Residue}
\usage{
ASR(row_cluster,col_cluster,W)
}
\arguments{
\item{row_cluster}{The cluster results of the rows of W, this value should be a vector whose length is the same as the number of rows in W}

\item{col_cluster}{The cluster results of the columns of W, this value should be a vector whose length is the same as the number of columns in W}

\item{W}{The matrix to be factorized}
}
\value{
The average residues of the bi-clustering results
}
\description{
To calculate average residues of the bi-clustering results
}
\examples{

W <- simu_data_generation()
OSNMTF_res <- OSNMTF(W,k=5,l=4)
row_cluster <- OSNMTF_res[[2]][[1]]
column_cluster <- OSNMTF_res[[2]][[2]]
ASR_value <- ASR(row_cluster,column_cluster,W)
}
\author{
Xiaoyao Yin
}

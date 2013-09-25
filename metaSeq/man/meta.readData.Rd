\name{meta.readData}
\alias{meta.readData}
\title{
readData function for meta-analysis
}
\description{
\code{\link[NOISeq]{readData}} function in NOISeq package customized for one-sided test in meta-analysis. Parallel computing is also available by snow package.
}
\usage{
meta.readData(data = NULL, factors = NULL, length = NULL, biotype = NULL, chromosome = NULL, gc = NULL, studies = NULL)
}
\arguments{
  \item{data}{
Matrix or data.frame containing the counts (or expression data) for each feature and sample. Features must be in rows and samples must be in columns.
}
  \item{factors}{
A data.frame containing the experimental condition or group for each sample (columns in the data object).
}
  \item{length}{
Optional argument.Vector, matrix or data.frame containing the length of each feature. In case of giving a vector, the names of the vector must be the feature names or ids with the same type of identifier used in data. If a matrix or a data.frame is provided, and it has two columns, it is expected that the feature names or ids are in the first column and the length of the features in the second. If it only has one column containing the length, the rownames of the object must be the feature names or ids.
}
  \item{biotype}{
Optional argument.Vector, matrix or data.frame containing the biological group (biotype) for each feature. In case of giving a vector, the names of the vector must be the feature names or ids with the same type of identifier used in data. If a matrix or a data.frame is provided, and it has two columns, it is expected that the feature names or ids are in the first column and the biotypes of the features in the second. If it only has one column containing the biotypes, the rownames of the object must be the feature names or ids.
}
  \item{chromosome}{
Optional argument. A matrix or data.frame containing the chromosome, start position and end position of each feature. The rownames must be the feature names or ids with the same type of identifier used in data.
}
  \item{gc}{
Optional argument.Vector, matrix or data.frame containing the GC content of each feature. In case of giving a vector, the names of the vector must be the feature names or ids with the same type of identifier used in data. If a matrix or a data.frame is provided, and it has two columns, it is expected that the feature names or ids are in the first column and the GC content of the features in the second. If it only has one column containing the GC content, the rownames of the object must be the feature names or ids.
}
  \item{studies}{
A vector specifying which column in data are measured in common study. Its length must be equal to the number of column in data.
}
}
\references{
Tarazona, S. and Garcia-Alcalde, F. and Dopazo, J. and Ferrer, A. and Conesa, A. (2011) Differential expression in RNA-seq: A matter of depth. \emph{Genome Research},
\bold{21(12)}: 2213-2223
}
\author{
Koki Tsuyuzaki, Itoshi Nikaido
}
\seealso{
\code{\link[NOISeq]{readData}}
}
\examples{
data(BreastCancer)
library("snow")

# Experimental condition (1: BreastCancer, 0: Normal)
flag1 <- c(1,1,1,0,0, 1,0, 1,1,1,1,1,1,1,0, 1,1,0)

# Source of data
flag2 <- c("A","A","A","A","A", "B","B", "C","C","C","C","C","C","C","C", "D","D","D")

# readData function for meta-analysis
cds <- meta.readData(data = BreastCancer, factor = flag1, studies = flag2)

# oneside NOISeq for meta-analysis
cl <- makeCluster(4, "SOCK")
result <- meta.oneside.noiseq(cds, k = 0.5, norm = "tmm", replicates = "biological", factor = flag1, conditions = c(1, 0), studies = flag2, cl = cl)
stopCluster(cl)

# Fisher's method (without weighting)
F <- Fisher.test(result)
str(F)

# Stouffer's method (with weighting by sample-size)
S <- Stouffer.test(result)
str(S)
}
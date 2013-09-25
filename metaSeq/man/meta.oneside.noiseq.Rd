\name{meta.oneside.noiseq}
\alias{meta.oneside.noiseq}
\title{
One-sided NOISeq for meta-analysis
}
\description{
NOISeq customized for one-sided test in meta-analysis. Parallel computing is also available by snow package.
}
\usage{
meta.oneside.noiseq(input, k = 0.5, norm = c("rpkm", "uqua", "tmm", "n"), replicates = c("technical", "biological", "no"), factor = NULL, conditions = NULL, pnr = 0.2, nss = 5, v = 0.02, lc = 1, studies = NULL, cl = NULL)
}
\arguments{
  \item{input}{
Object of eSet class coming from readData function or other R packages such as DESeq.
}
  \item{k}{
Counts equal to 0 are replaced by k. By default, k = 0.5.
}
  \item{norm}{
Normalization method. It can be one of "rpkm" (default), "uqua" (upper quartile), "tmm" (trimmed mean of M) or "n" (no normalization).
}
  \item{replicates}{
In this argument, the type of replicates to be used is defined. Technical, biological or none. By default, technical replicates option is chosen.

\strong{Note that "no" is automatically chosen against the study which has no replicate.}
}
  \item{factor}{
A string indicating the name of factor whose levels are the conditions to be compared.
}
  \item{conditions}{
A vector containing the two conditions to be compared by the differential expression algorithm (needed when the factor contains more than 2 different conditions).
}
  \item{pnr}{
Percentage of the total reads used to simulated each sample when no replicates are available. By default, pnr = 0.2.
}
  \item{nss}{
Number of samples to simulate for each condition (nss>= 2). By default, nss = 5.
}
  \item{v}{
Variability in the simulated sample total reads. By default, v = 0.02. Sample total reads is computed as a random value from a uniform distribution in the interval [(pnr-v)*sum(counts), (pnr+v)*sum(counts)]
}
  \item{lc}{
Length correction is done by dividing expression by length^lc. By default, lc = 1.
}
  \item{studies}{
A vector specifying which column in data are measured in common study. Its length must be equal to the number of column in data.
}
  \item{cl}{
cluster object in snow pacakge.
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
\code{\link[NOISeq]{noiseq}}
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
\name{Fisher.test}
\alias{Fisher.test}
\title{
Fisher's combined probability method
}
\description{
Fisher's method combines multiple p-values which are calculated in each study.
}
\usage{
Fisher.test(pvals, na.mode = "notignore")
}
\arguments{
  \item{pvals}{
A matrix coming from \code{\link{meta.oneside.noiseq}} function or \code{\link{other.oneside.pvalues}}, which is used for any one-sided p-values or probability.
}
  \item{na.mode}{
A string indicating how to treat NA in pvals. "notignore" means that genes having at least one NA is regarded as NA. "ignore" means NA is ignored and remaining data is used. By default, na.mode = "notignore".
}
}
\references{
Fisher, R. A. (1932) \emph{Statistical Methods for Research Workers}, \bold{4th edition}, Oliver and Boyd, London.
}
\author{
Koki Tsuyuzaki, Itoshi Nikaido
}
\seealso{
\code{\link{meta.readData}}, \code{\link{meta.oneside.noiseq}}, \code{\link{other.oneside.pvalues}}
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
# cl <- makeCluster(4, "SOCK")
# result <- meta.oneside.noiseq(cds, k = 0.5, norm = "tmm", replicates = "biological", factor = flag1, conditions = c(1, 0), studies = flag2, cl = cl)
# stopCluster(cl)

# Script above is very time-consumming step. Please use this pre-calculated result instead
data(Result.Meta)
result <- Result.Meta

# Fisher's method (without weighting)
F <- Fisher.test(result)
str(F)

# Stouffer's method (with weighting by sample-size)
S <- Stouffer.test(result)
str(S)
}
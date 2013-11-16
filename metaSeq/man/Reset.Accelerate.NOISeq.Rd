\name{Reset.Accelerate.NOISeq}
\alias{Reset.Accelerate.NOISeq}
\title{
Reset Accelerate.NOISeq function
}
\description{
Reseting the result of Accelerate.NOISeq function and making NOISeq as normal mode.
}
\usage{
Reset.Accelerate.NOISeq()
}
\author{
Koki Tsuyuzaki, Itoshi Nikaido
}
\examples{
library("NOISeq")

testdata <- matrix(rpois(30000*6,500),nrow=30000,ncol=6)
flag <- c(1,1,1,0,0,0)

# Normal NOIseq
before1 <- proc.time()
cds <- readData(testdata, factor = as.data.frame(flag))
cds <- noiseq(cds, k = 0.5, norm = "tmm", replicates = "biological", factor = "flag", conditions = c(1, 0))
after1 <- proc.time()

# Accelerated NOIseq
Accelerate.NOISeq()
before2 <- proc.time()
cds <- readData(testdata, factor = as.data.frame(flag))
cds <- noiseq(cds, k = 0.5, norm = "tmm", replicates = "biological", factor = "flag", conditions = c(1, 0))
after2 <- proc.time()

# Normal NOIseq
Reset.Accelerate.NOISeq()
before3 <- proc.time()
cds <- readData(testdata, factor = as.data.frame(flag))
cds <- noiseq(cds, k = 0.5, norm = "tmm", replicates = "biological", factor = "flag", conditions = c(1, 0))
after3 <- proc.time()

# Computational Time
after1 - before1
after2 - before2
after3 - before3
}
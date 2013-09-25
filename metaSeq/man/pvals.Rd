\name{pvals}
\alias{pvals}
\docType{data}
\title{
P-values or probability calculated by DESeq, edgeR, baySeq, NOISeq, and DEGseq against StudyA
}
\description{
P-values or probability calculated by \pkg{DESeq}, \pkg{edgeR}, \pkg{baySeq}, \pkg{NOISeq}, and \pkg{DEGSeq} against \code{\link{StudyA}}, which was down-sampled simulation data (1, 1/2, 1/4, 1/8, 1/16, and 1/32).
}
\usage{data(pvals)}
\source{
\url{https://trace.ddbj.nig.ac.jp/DRASearch/study?acc=SRP008746}
}
\seealso{
       \code{\link{StudyA}}, \code{\link{BreastCancer}}.
}
\examples{
data(pvals)
names(pvals)
}
\keyword{datasets}
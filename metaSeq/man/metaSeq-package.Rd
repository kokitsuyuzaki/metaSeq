\name{metaSeq-package}
\alias{metaSeq-package}
\alias{metaSeq}
\docType{package}
\title{
Meta-analysis of RNA-Seq count data in multiple studies
}
\description{
Meta-analysis for multiple studies's RNA-Seq count data. In this package, probability of gene differential is calculated by NOISeq, which is customized for one-sided test. One-sided probabilities are integrated by Fisher's method (without weighting) or Stouffer's method (with weighting by sample-size). P-values or probabilities calculated by non-NOISeq methods are also applicable by \code{other.oneside.pvalues}.
}
\details{
\tabular{ll}{
Package: \tab metaSeq\cr
Type: \tab Package\cr
Version: \tab 0.99.0\cr
Date: \tab 2013-09-25\cr
License: \tab Artistic-2.0\cr
}
}
\author{
Koki Tsuyuzaki, Itoshi Nikaido

Maintainer: Koki Tsuyuzaki <k.t.the-answer@hotmail.co.jp>
}
\references{
Tarazona, S. and Garcia-Alcalde, F. and Dopazo, J. and Ferrer, A. and Conesa, A. (2011) Differential expression in RNA-seq: A matter of depth. \emph{Genome Research},
\bold{21(12)}: 2213-2223
}
\keyword{ package }
\seealso{
\code{\link[NOISeq]{readData}}, \code{\link[NOISeq]{noiseq}}
}
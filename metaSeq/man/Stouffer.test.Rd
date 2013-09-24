\name{Stouffer.test}
\alias{Stouffer.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stouffer's weighted Z-score method (Inverse normal method)
}
\description{
Stouffer's method combines multiple weighted Z-scores which are calculated in each study. Although many weight can be introduced but weighting by sample-size is used in \code{\link{meta.oneside.noiseq}}.
}
\usage{
Stouffer.test(pvals, na.mode = "notignore")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvals}{
A matrix coming from \code{\link{meta.oneside.noiseq}} function or \code{\link{other.oneside.pvalues}}, which is used for any one-sided p-values or probability.
}
  \item{na.mode}{
A string indicating how to treat NA in pvals. "notignore" means that genes having at least one NA is regarded as NA. "ignore" means NA is ignored and remaining data is used. By default, na.mode = "notignore".
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Stouffer, S. A. and Suchman, E. A. and DeVinney, L. C. and Star, S. A. and Williams, R. M. Jr. (1949) \emph{The American Soldier}, \bold{Vol. 1} - Adjustment during Army Life. Princeton, Princeton University Press.
}
\author{
Koki Tsuyuzaki
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
F

# Stouffer's method (with weighting by sample-size)
S <- Stouffer.test(result)
str(S)
S

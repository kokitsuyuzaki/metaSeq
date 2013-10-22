\name{BreastCancer}
\alias{BreastCancer}
\docType{data}
\title{
Multiple RNA-Seq count data designed as Breast Cancer cell lines vs Normal cells
}
\description{
A data frame with 23368 rows (genes) with following 18 columns (samples).

All reads were measured by Illumina Genome Analyzer II or IIX, trimmed as 36 base, and mapped to the human reference genome hg19 as single-end. Each experimental design was restricted as Breast cancer cell vs Normal cell. Quality Control was performed by FastQC and samples whose quality scores were at least over 20 were choosed. Counts are quantified by HTSeq.
}
\usage{data(BreastCancer)}
\details{
StudyA: SRP008746
\itemize{
\item A_1: Breast Cancer (HCC1937), SRX099961, SRR350976

\item A_2: Breast Cancer (HCC3153), SRX101334, SRR353604_1

\item A_3: Breast Cancer (SUM131502), SRX101335, SRR353948_1

\item A_4: Normal (MCF10A), SRX099963, SRR353603_1

\item A_5: Normal (HCC2337), SRX101336, SRR353602_1
}

StudyB: SRP006726
\itemize{
\item B_1: Breast Cancer (HCC1954), SRX061997, SRR201983

\item B_2: Normal (HMEC), SRX061998, SRR201986
}

StudyC: SRP005601
\itemize{
\item C_1: Breast Cancer (BT20), SRX040501, SRR097786_1

\item C_2: Breast Cancer (BT474), SRX040502, SRR097787_1

\item C_3: Breast Cancer (MCF7), SRX040504, SRR097789_1

\item C_4: Breast Cancer (MDAMB231), SRX040505, SRR097790_1

\item C_5: Breast Cancer (MDAMB468), SRX040506, SRR097791_1

\item C_6: Breast Cancer (T47D), SRX040507, SRR097792_1

\item C_7: Breast Cancer (ZR751), SRX040508, SRR097793_1

\item C_8: Normal (MCF10A), SRX040503, SRR097788_1
}

StudyD: ERP000992
\itemize{
\item D_1: Breast Cancer (MCF7), ERX030989, ERR053953

\item D_2: Breast Cancer (T47D), ERX031000, ERR053958

\item D_3: Normal (Ishikawa), ERX030994, ERR053948
}
}
\source{
\url{https://trace.ddbj.nig.ac.jp/DRASearch/study?acc=SRP008746}

\url{http://trace.ddbj.nig.ac.jp/DRASearch/study?acc=SRP006726}

\url{http://trace.ddbj.nig.ac.jp/DRASearch/study?acc=SRP005601}

\url{http://trace.ddbj.nig.ac.jp/DRASearch/study?acc=ERP000992}
}
\references{
Hon, G. C. and Hawkins, R. D. and Caballero, O. L. and Lo, C et al. (2012) Global DNA hypomethylation coupled to repressive chromatin domain foormation and gene silencing in breast cancer. \emph{Genome Research}, \bold{22 (2)}: 246-258

Sun, Z. and Asmann, Y. W. and Kalari, K. R. and Bot, B. et al. (2011) Integrated analysis of gene expression, CpG island methylation, and gene copy number in breast cancer cells by deep sequencing. \emph{PLOS ONE}, \bold{25;6(2)}: e17490
}
\seealso{
       \code{\link{StudyA}}, \code{\link{pvals}}.
}
\examples{
data(BreastCancer)
}
\keyword{datasets}
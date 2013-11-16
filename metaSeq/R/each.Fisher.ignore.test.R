each.Fisher.ignore.test <-
function (p) 
{
    loc <- setdiff(1:length(p), which(is.na(p)))
    if (length(loc) >= 1) {
        p <- p[loc]
        Xsq <- -2 * sum(log(p))
        pval <- 1 - pchisq(Xsq, df = 2 * length(p))
        return(pval)
    }
    else {
        return(NA)
    }
}

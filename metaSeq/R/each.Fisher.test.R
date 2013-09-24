each.Fisher.test <-
function (p) 
{
    Xsq <- -2 * sum(log(p))
    pval <- 1 - pchisq(Xsq, df = 2 * length(p))
    return(pval)
}

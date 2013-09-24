each.Stouffer.test <-
function (p, w) 
{
    if (missing(w)) {
        w <- rep(1, length(p))/length(p)
    }
    else {
        if (length(w) != length(p)) 
            stop("Length of p and w must equal!")
    }
    Zi <- qnorm(1 - p)
    Z <- sum(w * Zi)/sqrt(sum(w^2))
    pval <- 1 - pnorm(Z)
    return(pval)
}

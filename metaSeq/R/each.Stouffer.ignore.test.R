each.Stouffer.ignore.test <-
function (p, w) 
{
    if (missing(w)) {
        w <- rep(1, length(p))/length(p)
    }
    else {
        if (length(w) != length(p)) 
            stop("Length of p and w must equal!")
    }
    loc <- setdiff(1:length(p), which(is.na(p)))
    if (length(loc) >= 1) {
        p <- p[loc]
        w <- w[loc]
        Zi <- qnorm(1 - p)
        Z <- sum(w * Zi)/sqrt(sum(w^2))
        pval <- 1 - pnorm(Z)
        return(pval)
    }
    else {
        return(NA)
    }
}

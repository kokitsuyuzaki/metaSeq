oneside.noiseq <-
function (input, k = 0.5, norm = c("rpkm", "uqua", "tmm", "n"), 
    replicates = c("technical", "biological", "no"), factor = NULL, 
    conditions = NULL, pnr = 0.2, nss = 5, v = 0.02, lc = 1, 
    x = NULL) 
{
    env <- getNamespace("NOISeq")
    assignInNamespace("probdeg", metaSeq:::custom.probdeg, ns = "NOISeq", 
        envir = env)
    assignInNamespace("MD", metaSeq:::custom.MD, ns = "NOISeq", 
        envir = env)
    k2 = 0.5
    norm2 = c("rpkm", "uqua", "tmm", "n")
    replicates2 = c("technical", "biological", "no")
    factor2 = NULL
    conditions2 = NULL
    pnr2 = 0.2
    nss2 = 5
    v2 = 0.02
    lc2 = 1
    if (k != 0.5) {
        k2 <- k
    }
    if (length(norm) != 4) {
        norm2 <- norm
    }
    if (length(replicates) != 3) {
        replicates2 <- replicates
    }
    if (!is.null(factor)) {
        factor2 <- factor
    }
    if (!is.null(conditions)) {
        conditions2 <- conditions
    }
    if (pnr != 0.2) {
        pnr2 <- pnr
    }
    if (nss != 5) {
        nss2 <- nss
    }
    if (v != 0.02) {
        v2 <- v
    }
    if (lc != 1) {
        lc2 <- lc
    }
    e2 <<- new.env()
    e2$k <- k2
    e2$norm <- norm2
    e2$replicates <- replicates2
    e2$factor <- factor2
    e2$conditions <- conditions2
    e2$pnr <- pnr2
    e2$nss <- nss2
    e2$v <- v2
    e2$lc <- lc2
    fff <- eval(parse(text = eval(parse(text = "e2$factor"))))
    A <- length(which(conditions[1] == fff))
    B <- length(which(conditions[2] == fff))
    if ((A == 1) && (B == 1)) {
        warning("Your dataset contains some non-replicated sample. \"replicates\" parameter is automatically selected as \"no\".\n")
        out <- NOISeq::noiseq(input, k = e2$k, norm = e2$norm, 
            replicates = "no", factor = e2$factor, conditions = e2$conditions, 
            pnr = e2$pnr, nss = e2$nss, v = e2$v, lc = e2$lc)
    }
    else {
        out <- NOISeq::noiseq(input, k = e2$k, norm = e2$norm, 
            replicates = e2$replicates, factor = e2$factor, conditions = e2$conditions, 
            pnr = e2$pnr, nss = e2$nss, v = e2$v, lc = e2$lc)
    }
    return(out)
    assignInNamespace("probdeg", metaSeq:::original.probdeg, 
        ns = "NOISeq", envir = env)
    assignInNamespace("MD", metaSeq:::original.MD, ns = "NOISeq", 
        envir = env)
}

original.MD <-
function (dat = dat, selec = c(1:nrow(dat))) 
{
    pares <- as.matrix(combn(ncol(dat), 2))
    if (NCOL(pares) > 30) {
        sub30 <- sample(1:NCOL(pares), size = 30, replace = FALSE)
        pares <- pares[, sub30]
    }
    mm <- NULL
    dd <- NULL
    for (i in 1:ncol(pares)) {
        a <- dat[selec, pares[1, i]]
        b <- dat[selec, pares[2, i]]
        mm <- cbind(mm, log(a/b, 2))
        dd <- cbind(dd, abs(a - b))
    }
    list(M = mm, D = dd)
}

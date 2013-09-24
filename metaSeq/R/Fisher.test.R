Fisher.test <-
function (pvals, na.mode = "notignore") 
{
    l <- ncol(pvals)
    U <- pvals[1, 1][[1]]
    L <- pvals[2, 1][[1]]
    weight <- unlist(pvals["weight", ])
    for (i in 2:l) {
        U <- cbind(U, pvals[1, i][[1]])
        L <- cbind(L, pvals[2, i][[1]])
    }
    up <- c()
    low <- c()
    if (na.mode == "notignore") {
        up <- apply(U, 1, each.Fisher.test)
        low <- apply(L, 1, each.Fisher.test)
    }
    if (na.mode == "ignore") {
        up <- apply(U, 1, each.Fisher.ignore.test)
        low <- apply(L, 1, each.Fisher.ignore.test)
    }
    else {
        warnings("You have to specify na.mode as \"ignore\" or \"notignore\"")
    }
    list(Upper = up, Lower = low, Weight = weight)
}

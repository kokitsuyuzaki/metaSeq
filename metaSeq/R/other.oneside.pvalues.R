other.oneside.pvalues <-
function (Upper, Lower, weight = NULL) 
{
    if ((min(!is.na(Upper)) < 0) || (max(!is.na(Upper)) > 1)) {
        stop("Is this dataset (upper) pvalues? Some elements exceed 0 - 1 range. Please confirm first.\n")
    }
    if ((min(!is.na(Lower)) < 0) || (max(!is.na(Lower)) > 1)) {
        stop("Is this dataset (lower) pvalues? Some elements exceed 0 - 1 range. Please confirm first.\n")
    }
    if (nrow(Upper) != nrow(Lower)) {
        stop("Number of rows in upper p-values and lower p-values are different!\n")
    }
    if (ncol(Upper) != ncol(Lower)) {
        stop("Number of columns in upper p-values and lower p-values are different!\n")
    }
    if ((!is.null(weight)) && (ncol(Upper) != length(weight))) {
        A <- ncol(Upper)
        B <- length(weight)
        Call <- paste0("Number of column in p-value matrix is ", 
            A, " but length of weight vector is ", B, ". Please confirm first.\n")
        stop(Call)
    }
    l <- ncol(Upper)
    if (is.null(weight)) {
        out <- sapply(1:l, function(x) {
            output <- list()
            length(output) <- 3
            names(output) <- c("upper", "lower", "weight")
            output$upper <- Upper[, x]
            output$lower <- Lower[, x]
            return(output)
        })
        colnames(out) <- paste("Exp", 1:l)
        return(out)
    }
    else {
        out <- sapply(1:l, function(x) {
            output <- list()
            length(output) <- 3
            names(output) <- c("upper", "lower", "weight")
            output$upper <- Upper[, x]
            output$lower <- Lower[, x]
            output$weight <- weight[x]
            return(output)
        })
        colnames(out) <- paste("Exp", 1:l)
        return(out)
    }
}

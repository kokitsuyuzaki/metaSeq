meta.readData <-
function (data = NULL, factors = NULL, length = NULL, biotype = NULL, 
    chromosome = NULL, gc = NULL, studies = NULL) 
{
    if (is.null(studies)) {
        stop("Please specify \"studies\" at first!\n")
    }
    else if (!is.vector(factors)) {
        stop("Please \"factors\" parameter as vector.\n")
    }
    else if (!is.vector(studies)) {
        stop("Please \"studies\" parameter as vector.\n")
    }
    else if (length(factors) != length(studies)) {
        stop("Length of factors and that of studies are different!\n")
    }
    else {
        out <- list()
        l <- nlevels(as.factor(studies))
        length(out) <- l
        length2 <- NULL
        biotype2 <- NULL
        chromosome2 <- NULL
        gc2 <- NULL
        if (!is.null(length)) {
            length2 <- length
        }
        if (!is.null(biotype)) {
            biotype2 <- biotype
        }
        if (!is.null(chromosome)) {
            chromosome2 <- chromosome
        }
        if (!is.null(gc)) {
            gc2 <- gc
        }
        e <<- new.env()
        e$factors <- factors
        e$studies <- studies
        e$length <- length2
        e$biotype <- biotype2
        e$chromosome <- chromosome2
        e$gc <- gc2
        loc <- list()
        length(loc) <- l
        e$loc <- loc
        for (x in 1:l) {
            loc[[x]] <- which(e$studies == levels(as.factor(e$studies))[x])
            e$loc[[x]] <- loc[[x]]
            out[[x]] <- readData(data = data[, e$loc[[x]]], factors = as.data.frame(e$factors[e$loc[[x]]]), 
                length = e$length, biotype = e$biotype, chromosome = e$chromosome, 
                gc = e$gc)
        }
    }
    class(out) <- "metaExpressionSet"
    return(out)
}

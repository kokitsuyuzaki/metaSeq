Reset.Accelerate.NOISeq <-
function () 
{
    env <- getNamespace("NOISeq")
    assignInNamespace("busca", original.busca, ns = "NOISeq", 
        envir = env)
    assignInNamespace("n.menor", original.n.menor, ns = "NOISeq", 
        envir = env)
}

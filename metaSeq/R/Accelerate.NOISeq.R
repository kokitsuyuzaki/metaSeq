Accelerate.NOISeq <-
function () 
{
    data(text.n.menor)
    data(text.busca)
    env <- getNamespace("NOISeq")
    sourceCpp(code = text.n.menor)
    sourceCpp(code = text.busca)
    assignInNamespace("busca", busca, ns = "NOISeq", envir = env)
    assignInNamespace("n.menor", nmenor, ns = "NOISeq", envir = env)
}

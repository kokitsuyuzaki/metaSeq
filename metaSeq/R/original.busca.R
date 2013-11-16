original.busca <-
function (x, S) 
{
    which(S[, 1] == x[1] & S[, 2] == x[2])
}

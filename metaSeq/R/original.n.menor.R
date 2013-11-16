original.n.menor <-
function (x, S1, S2) 
{
    length(which(S1 <= x[1] & S2 <= x[2]))
}

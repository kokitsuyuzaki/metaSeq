######### testdata ########
testdata <- matrix(rpois(12*100, 500), ncol=12, nrow=100)
test.flag1 <- c(1,1,1,0,0,0, 1,1,1,0,0,0)
test.flag2 <- c("A","A","A","A","A","A", "B","B","B","B","B","B")

######### readData, meta.readData ########
cds11 <- NOISeq:::readData(data = testdata[, 1:6], factor = as.data.frame(test.flag1[1:6]))
cds12 <- NOISeq:::readData(data = testdata[, 7:12], factor = as.data.frame(test.flag1[7:12]))
cds13 <- NOISeq:::readData(data = testdata[, c(1,4)], factor = as.data.frame(test.flag1[c(1,4)]))
cds14 <- NOISeq:::readData(data = testdata[, c(1,4:5)], factor = as.data.frame(test.flag1[c(1,4:5)]))

cds2 <- meta.readData(data = testdata, factor = test.flag1, studies = test.flag2)

expect_equivalent(prod(dim(cds11)), 600)
expect_equivalent(prod(dim(cds12)), 600)
expect_equivalent(prod(dim(cds13)), 200)
expect_equivalent(prod(dim(cds14)), 300)

expect_equivalent(prod(dim(cds2[[1]])), 600)
expect_equivalent(prod(dim(cds2[[2]])), 600)

######### noiseq ########
# Warningが出て、NOISeqBIOを推奨してくる
result11 <- NOISeq:::noiseq(cds11, k = 0.5, norm = "tmm", replicates = "biological", factor = "test.flag1[1:6]", conditions = c(1, 0))
# Warningが出て、NOISeqBIOを推奨してくる
result12 <- NOISeq:::noiseq(cds12, k = 0.5, norm = "tmm", replicates = "biological", factor = "test.flag1[7:12]", conditions = c(1, 0))
# Warningが出ない
result13 <- NOISeq:::noiseq(cds13, k = 0.5, norm = "tmm", replicates = "no", factor = "test.flag1[c(1, 4)]", conditions = c(1, 0))
# Warningが出て、NOISeqBIOを推奨してくる
result14 <- NOISeq:::noiseq(cds14, k = 0.5, norm = "tmm", replicates = "biological", factor = "test.flag1[c(1, 4:5)]", conditions = c(1, 0))
expect_equivalent(is(result11)[1], "Output")
expect_equivalent(is(result12)[1], "Output")
expect_equivalent(is(result13)[1], "Output")
expect_equivalent(is(result14)[1], "Output")
expect_equivalent(is(result11)[2], "myInfo")
expect_equivalent(is(result12)[2], "myInfo")
expect_equivalent(is(result13)[2], "myInfo")
expect_equivalent(is(result14)[2], "myInfo")

######## oneside.noiseq #########
# 環境で渡さないといけない気持ち悪い仕様
e <- new.env()
e$factors <- test.flag1
e$loc[[1]] <- 1:6
e$loc[[2]] <- 7:12
e$loc[[3]] <- c(1,4)

result21 <- metaSeq:::oneside.noiseq(cds2[[1]], k = 0.5, norm = "tmm", replicates = "biological", factor = "e$factors[e$loc[[x]]]", conditions = c(1, 0), x = 1)
result22 <- metaSeq:::oneside.noiseq(cds2[[2]], k = 0.5, norm = "tmm", replicates = "biological", factor = "e$factors[e$loc[[x]]]", conditions = c(1, 0), x = 2)

expect_equivalent(dim(result11@results[[1]]), dim(result21@results[[1]]))
expect_equivalent(dim(result12@results[[1]]), dim(result22@results[[1]]))

######## oneside.noiseqbio #########
# metaSeq:::oneside.noiseqbio
result31 <- oneside.noiseqbio(cds2[[1]], k = 0.5, norm = "tmm", factor = "e$factors[e$loc[[x]]]", lc=1, r = 20, adj = 1.5, plot = FALSE, a0per = 0.9, random.seed = 12345, fileter = 2)


######### other.oneside.pvalues ########
upper <- matrix(runif(300), ncol=3, nrow=100)
lower <- matrix(runif(300), ncol=3, nrow=100)
weight <- c(3,6,8)
result3 <- other.oneside.pvalues(upper, lower, weight)

expect_equivalent(length(result3), 9)
expect_equivalent(length(result3[,1]), 3)
expect_equivalent(length(result3[,2]), 3)
expect_equivalent(length(result3[,3]), 3)
expect_equivalent(length(result3[,1][[1]]), 100)
expect_equivalent(length(result3[,1][[2]]), 100)
expect_equivalent(length(result3[,1][[3]]), 1)
expect_equivalent(length(result3[,2][[1]]), 100)
expect_equivalent(length(result3[,2][[2]]), 100)
expect_equivalent(length(result3[,2][[3]]), 1)
expect_equivalent(length(result3[,3][[1]]), 100)
expect_equivalent(length(result3[,3][[2]]), 100)
expect_equivalent(length(result3[,3][[3]]), 1)

######### meta.oneside.noiseq ########
result4 <- meta.oneside.noiseq(cds2, k = 0.5, norm = "tmm", replicates = "biological", factor = test.flag1, conditions = c(1, 0), studies = test.flag2)
expect_equivalent(ncol(result4), 2)

######### parallel computing (snow) ########
cl <- snow:::makeCluster(4, "SOCK")
result5 <- meta.oneside.noiseq(cds2, k = 0.5, norm = "tmm", replicates = "biological", factor = test.flag1, conditions = c(1, 0), studies = test.flag2, cl = cl)
stopCluster(cl)
expect_equivalent(ncol(result5), 2)

######### Fisher / Stouffer ########
F <- Fisher.test(result4)
S <- Stouffer.test(result4)
expect_equivalent(length(F$Upper), 100)
expect_equivalent(length(S$Upper), 100)
expect_equivalent(length(F$Lower), 100)
expect_equivalent(length(S$Lower), 100)
expect_equivalent(length(F$Weight), 2)
expect_equivalent(length(S$Weight), 2)

# WeightなしにStouffer.testをやるとエラー
result6 <- other.oneside.pvalues(upper, lower)
expect_error(Stouffer.test(result6))


# Discriminative PCA (https://arxiv.org/abs/1710.09429)

dPCA <- function(X, Y, k) eigen(solve(cov(Y)) %*% cov(X))$vectors[,1:k]

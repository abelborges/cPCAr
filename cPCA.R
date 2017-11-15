# Contrastive PCA for a given alpha
# X, Y: datasets (rows = sample units, cols = variables)
# alpha: hyper-parameter, controls the contrastiveness
# k: number of desired (reduced) dimensions
cPCA_alpha <- function(X, Y, alpha, k)
  eigen(cor(X) - alpha * cor(Y))$vectors[,1:k]

# principal angles between subspaces
PABS <- function(U, V) {
  if(nrow(U) != nrow(V)) stop('# of basis vectors in U and V must equal')
  ips <- matrix(0, ncol(U), ncol(V))
  for(i in 1:ncol(U)) {
    for(j in 1:ncol(V)) ips[i,j] <- as.numeric(U[,i] %*% V[,j])
  }
  svd(ips)$d
}

# fully-connected spectral clustering
# TODO: include normalized version
spectral_clustering <- function(D, p, normalized=FALSE) {
  if(p >= nrow(D)) stop('p must be < dimension of D')
  kmeans(eigen(diag(rowSums(D)) - D)$vectors[,1:p], p)$cluster
}

logspace <- function(from, to, n) exp(seq(log(from), log(to), len=n))

# cPCA
cPCA <- function(X, Y, alphas=logspace(0.1, 1000, 40), k=2, p=4) {
  V <- vector('list', length(alphas))
  for(i in seq_along(alphas)) V[[i]] <- cPCA_alpha(X, Y, alphas[i], k)
  D <- matrix(0, length(alphas), length(alphas))
  for(i in seq_along(alphas)) {
    D[i,i] <- 0
    if(i < length(alphas)) {
      for(j in (i+1):length(alphas)) D[i,j] <- D[j,i] <- prod(PABS(V[[i]], V[[j]]))
    }
  }
  clusters <- spectral_clustering(D, p)
  D_rs <- rowSums(D)
  alpha_star <- numeric(p)
  V_star <- vector('list', p)
  for(i in 1:p) {
    cl <- which(clusters == i)
    i_star <- cl[which.max(D_rs[cl])]
    V_star[[i]] <- V[[i_star]]
    alpha_star[i] <- alphas[i_star]
  }
  
  list(V_star = V_star, alpha_star = alpha_star)
}


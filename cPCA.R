# Contrastive PCA for a given alpha
# X, Y: datasets (rows = sample units, cols = variables)
# alpha: hyper-parameter, controls the degree of contrastiveness
# k: number of desired (reduced) dimensions
cPCA_alpha <- function(X, Y, alpha, k)
  eigen(cor(X) - alpha * cor(Y))$vectors[,1:k]

# principal angles between subspaces
PABS <- function(U, V) {
  if(!all.equal(dim(U)) || !all.equal(dim(V)))
    stop('matrices must be square')
  ips <- matrix(0, ncol(U), ncol(V))
  for(i in 1:ncol(U)) {
    for(j in 1:ncol(V)) ips[i,j] <- as.numeric(U[,i] %*% V[,j])
  }
  svd(ips)$d
}

# fully-connected (W = D) unnormalized (L = diag(rowsum(D)) - W) spec. clust.
unnormalized_spectral_clustering <- function(D, p) {
  if(p <= nrow(D)) stop('p must be < dimension of D')
  if(!all.equal(dim(D))) stop('D must be square')
  U <- eigen(diag(rowsum(D)) - D)$vectors[,1:p]
  kmeans(U, p)$cluster
}

# cPCA
cPCA <- function(X, Y, alphas, k, p=4) {
  V <- vector('list', length(alphas))
  for(i in seq_along(alphas)) V[[i]] <- cPCA_alpha(X, Y, alphas[i], k)
  D <- matrix(0, k, k)
  for(i in seq_along(alphas)) {
    D[i,i] <- 0
    for(j in (i+1):length(alphas)) {
      thetas <- PABS(V[[i]], V[[j]])
      D[i,j] <- D[j,i] <- prod(thetas)
    }
  }
  clusters <- unnormalized_spectral_clustering(D, p)
  D_rs <- rowsum(D)
  alpha_star <- numeric(p)
  V_star <- vector('list', p)
  for(i in 1:p) {
    cl <- which(clusters == i)
    i_star <- cl[which.max(D_rs[cl])]
    V_star[i] <- V[[i_star]]
    alpha_star[i] <- alphas[i_star]
  }
  
  list(V_star = V_star, alpha_star = alpha_star)
}


# Contrastive PCA for a given alpha
# X, Y: datasets (rows = sample units, cols = variables)
# alpha: hyper-parameter, controls the contrastiveness
# k: number of desired (reduced) dimensions (components)
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

# spectral clustering algorithm from A. Y. Ng et al (2002)
spectral_clustering <- function(A, p) {
  if(p >= nrow(A)) stop('p must be < dimension of A')
  D_inv_sqrt <- diag(1/sqrt(rowSums(A)))
  L <- D_inv_sqrt %*% A %*% D_inv_sqrt
  kmeans(eigen(L)$vectors[,1:p], p)$cluster
}

logspace <- function(from, to, n) exp(seq(log(from), log(to), len=n))

# cPCA
cPCA <- function(X, Y, p, alphas=logspace(0.1, 1000, 40), k=2) {
  V <- vector('list', length(alphas))
  for(i in seq_along(alphas)) V[[i]] <- cPCA_alpha(X, Y, alphas[i], k)
  D <- matrix(0, length(alphas), length(alphas))
  for(i in seq_along(alphas)) {
    D[i,i] <- 0
    if(i < length(alphas)) {
      for(j in (i+1):length(alphas)) {
        thetas <- PABS(V[[i]], V[[j]])
        D[i,j] <- D[j,i] <- prod(cos(thetas))
      }
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


# Synthetic data from Appendix A of the paper
source('./cPCA.R')

X_red <- cbind(matrix(rnorm(20*100), nrow=100),
               matrix(rnorm(10*100, sd=sqrt(10)), nrow=100))
X_blue <- cbind(matrix(rnorm(10*100), nrow=100),
                matrix(rnorm(10*100, mean=3), nrow=100),
                matrix(rnorm(10*100, sd=sqrt(10)), nrow=100))
X_yellow <- cbind(matrix(rnorm(10*100, mean=6), nrow=100),
                  matrix(rnorm(10*100), nrow=100),
                  matrix(rnorm(10*100, sd=sqrt(10)), nrow=100))
X_black <- cbind(matrix(rnorm(10*100, mean=6), nrow=100),
                 matrix(rnorm(10*100, mean=3), nrow=100),
                 matrix(rnorm(10*100, sd=sqrt(10)), nrow=100))
X <- rbind(X_red, X_blue, X_yellow, X_black)
Y <- cbind(matrix(rnorm(10*400, sd=sqrt(3)), nrow=400),
           matrix(rnorm(10*400), nrow=400),
           matrix(rnorm(10*400, sd=sqrt(10)), nrow=400))

cpca <- cPCA(X, Y)

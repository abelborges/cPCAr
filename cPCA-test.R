# Synthetic data from Appendix A of the paper
source('./cPCA.R')
source('./dPCA.R')
source('./multiplot.R')
library(ggplot2)

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

make_viz <- function(alphas, Vs, groups) {
  gg <- vector('list', length(alphas))
  for(i in seq_along(gg)) {
    df <- data.frame(X %*% Vs[[i]], group = rep(groups, each = 100))
    gg[[i]] <- ggplot(df, aes(X1, X2, color = group)) +
      geom_point() +
      scale_color_manual(values = groups, labels = groups) +
      ggtitle(bquote(alpha ~ '=' ~ .(alphas[i]))) +
      labs(x = 'cPC1', y = 'cPC2')
  }
  multiplot(plotlist = gg, cols = 2)
}

groups <- c('red', 'blue', 'yellow', 'black')

# TODO: this results are different from the paper
alphas <- c(0, 0.7, 2.7, 119.4)
Vs <- vector('list', length(alphas))
for(i in seq_along(Vs)) Vs[[i]] <- cPCA_alpha(X, Y, alphas[i], 2)
make_viz(alphas, Vs, groups)

# TODO: make auto-selection work
cpca <- cPCA(X, Y, p = 4)
make_viz(cpca$alpha_star, cpca$V_star, groups)





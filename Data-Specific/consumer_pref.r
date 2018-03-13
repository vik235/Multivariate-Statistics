####
#### Reproduction of data from Johnson and Wichern Example 9.3. Each of 100 customers 
#### were asked to rate several attributes of a new product on a 7-point scale. The book 
#### only listed the sample correlation matrix, so we will simulate some actual data.
####

p <- 5
R <- matrix(c(1, 0.02, 0.96, 0.42, 0.01, 0.02, 1, 0.13, 0.71, 0.85, 0.96, 0.13, 1, 0.5, 
  0.11, 0.42, 0.71, 0.50, 1, 0.79, 0.01, 0.85, 0.11, 0.79, 1), nrow = p)

set.seed(101)
library(MASS)

n <- 100
X <- mvrnorm(n, mu = rep(0, p), Sigma = R)
colnames(X) <- c("Taste", "Value", "Flavor", "Snack", "Energy")

write.csv(X, "consumer_pref.csv", row.names = FALSE)

##
## Analysis.
##

library(Hotelling)
library(DescTools)

## Load simulated data.
X <- read.csv("consumer_pref.csv")
n <- nrow(X)
p <- ncol(X)

## Summary statistics.
x_bar <- colMeans(X)
S <- var(X)
R <- cor(X)

## Use QQ plots to investigate normality.
par(mfrow = c(2, 3))
qqnorm(X[, 1]); qqline(X[, 1])
qqnorm(X[, 2]); qqline(X[, 2])
qqnorm(X[, 3]); qqline(X[, 3])
qqnorm(X[, 4]); qqline(X[, 4])
qqnorm(X[, 5]); qqline(X[, 5])

## T^2-based 95% confidence region has primary axes equal to the eigenvectors of S and 
## half-lengths proportional to the square root of the eigenvalues of S.
eigen_S <- eigen(S)
primary_axes <- eigen_S$vectors
half_lengths <- sqrt(eigen_S$values) * sqrt(((p * (n - 1)) / (n * (n - p))) * 
  qf(0.95, p, n - p))

## Bonferroni-based 95% simultaneous confidence intervals for each of the p variables.
bonf_intervals <- matrix(NA, nrow = p, ncol = 2)
for(i in 1:p)
  bonf_intervals[i, ] <- x_bar[i] + c(-1, 1) * qt(1 - 0.025 / p, n - 1) * 
    sqrt(S[i, i] / n)
    
## Parametric p-value for testing H_0: mu' = (0, 0, 0, 0, 0).
mu_0 <- rep(0, p)
T2_obs <- drop(n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0))
p_val <- 1 - pf(((n - p) / ((n - 1) * p)) * T2_obs, p, n - p)
HotellingsT2Test(X, mu = mu_0)

## Bootstrap-based p-value for testing H_0: mu' = (0, 0, 0, 0, 0).
B <- 1000
T2_b <- numeric(B)
set.seed(101)
X_0 <- scale(X, center = TRUE, scale = FALSE)
for(b in 1:B) {
  X_b <- X_0[sample(1:n, replace = TRUE), ]
  x_bar_b <- colMeans(X_b)
  S_b <- var(X_b)
  T2_b[b] <- n * t(x_bar_b - mu_0) %*% solve(S_b) %*% (x_bar_b - mu_0)
}
p_val_boot <- sum(T2_b >= T2_obs) / B

##
## Factor analysis with varimax rotation.
##

fa <- factanal(covmat = R, factors = 2)
print(loadings(fa), cutoff = 0.25)


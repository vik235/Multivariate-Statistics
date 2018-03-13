####
#### The bootstrap is a resampling technique that allows for the approximation of the
#### sampling distribution and standard error of a statistic. It is especially useful in 
#### small-sample settings and other settings in which parametric results are not 
#### available.
####

library(boot)
library(mvtnorm)
library(Hotelling)
library(DescTools)

##
## Example: Inference on a univariate mean.
##

n <- 10
B <- 100
N <- 1000

## When H_0 is true, the t statistic is distributed as t with n - 1 degrees of freedom. 
## Below, we illustrate under the scenario that H_0 is indeed true. One implication is 
## that we should only get p-values < 0.05 for 5% of samples. Below, we simulate many 
## samples, so we can compare the parametric approach to the bootstrap.
mu_0 <- 0

sim_f <- function(mu, mu_0 = 0) {
  p_value_par <- p_value_boot <- rep(NA, N)
  for(i in 1:N) {
    if(i %% 10 == 0)
      cat(".")

    ## Simulate a sample of size n and compute its t statistic.
    y <- rnorm(n, mu)
    t_0 <- (mean(y) - mu_0) / (sqrt(var(y) / n))

    ## Bootstrap by repeatedly sampling with replacement, enforcing H_0 to be true each 
    ## time. Compute t for each bootstrapped sample.
    t_B <- rep(NA, B)
    for(b in 1:B) {
      y_b <- sample(y - mean(y), replace = TRUE)
      t_B[b] <- (mean(y_b) - mu_0) / (sqrt(var(y_b) / n))
    }

    ## Compute p-values. The parametric test uses the t distribution. The bootstrap test 
    ## p-value is the proportion of bootstrapped test statistics that are at least as  
    ## extreme as the observed one.
    p_value_par[i] <- 2 * (1 - pt(abs(t_0), n - 1))
    p_value_boot[i] <- mean(abs(t_B) >= abs(t_0))
  }
  
  return(list("p_values_par" = p_value_par, "p_values_boot" = p_value_boot))
}

## The bootstrap properly maintains the Type I error rate at alpha.
H_0_out <- sim_f(mu = 0)
mean(H_0_out$p_values_par < 0.05)
mean(H_0_out$p_values_boot < 0.05)

## When parametric assumptions hold, parametric tests will have higher power than non-
## parametric tests like the bootstrap.
H_a_out <- sim_f(mu = 1)
mean(H_a_out$p_values_par < 0.05)
mean(H_a_out$p_values_boot < 0.05)

## The 'boot' function automates the bootstrapping procedure and can incorporate 
## arbitrary statistics. Here, we approximate the sampling distribution of the inter-
## quartile range (the difference between the 75th and 25th percentiles). 
y <- rnorm(n)

stat_f <- function(y, ii) {
  return(diff(quantile(y[ii], c(0.25, 0.75))))
}
boot_out <- boot(y, stat_f, R = 1000)
hist(boot_out$t, main = "Estimated Sampling Distribution of IQR")

##
## Example: Inference on a multivariate mean.
##

n <- 10
p <- 3
B <- 500
N <- 1000
mu_0 <- rep(0, p)
rho <- 0.8
Sigma <- matrix(rho, nrow = p, ncol = p); diag(Sigma) <- 1

## Generate multivariate t data with mean 0 and degrees of freedom 5.
X <- rmvt(n, sigma = Sigma, df = 1, delta = rep(0, p))

## Multivariate normality does not hold.
par(mfrow = c(1, 3))
qqnorm(X[, 1], main = "Variable 1"); qqline(X[, 1])
qqnorm(X[, 2], main = "Variable 2"); qqline(X[, 2])
qqnorm(X[, 3], main = "Variable 3"); qqline(X[, 3])

## A function to compute T2.
T2_f <- function(X, mu_0) {
  ## The covariance matrices under the null and unrestricted scenarios.
  S <- ((n - 1) / n) * var(X)
  S_0 <- matrix(0, nrow = p, ncol = p)
  for(j in 1:n)
    S_0 <- S_0 + (X[j, ] - mu_0) %*% t(X[j, ] - mu_0) / n

  ## Compute T2 if the sample covariance matrices are non-singular.
  T2 <- NA
  if(det(S) > 0 & det(S_0) > 0) {
    Lambda <- (det(S) / det(S_0)) ^ (n / 2)
    T2 <- (n - 1) * (1 / (Lambda ^ (2 / n)) - 1)
  }

  return(T2)
}

## Function to simulate many datasets from the multivariate t distribution and compare 
## the parametric approach to the bootstrap.
sim_f <- function(mu, mu_0 = rep(0, p)) {
  p_value_par <- p_value_boot <- rep(NA, N)
  for(i in 1:N) {
    if(i %% 10 == 0)
      cat(".")

    ## Simulate a sample from the multivariate t.
    X <- rmvt(n, sigma = Sigma, df = 1, delta = mu)

    ## Observed value of T2.
    T2_0 <- T2_f(X, mu_0)
    T2_0_scaled <- (n - p) / ((n - 1) * p) * T2_0

    ## Bootstrap versions of T2. We first subtract the column means to force H_0 to be 
    ## true. We then sample *rows* with replacement to create a bootstrapped version of the 
    ## data, under the condition that H_0 is true. We sample rows in order to preserve the 
    ## correlations between the variables. 
    T2_b <- rep(NA, B)
    X_0 <- scale(X, center = TRUE, scale = FALSE) + kronecker(rep(1, n), t(mu_0))
    for(b in 1:B) {
      ii <- sample(1:n, replace = TRUE)
      T2_b[b] <- T2_f(X_0[ii, ], mu_0)
    }
    T2_b_scaled <- (n - p) / ((n - 1) * p) * T2_b

    ## Compute p-values. The parametric test uses the F distribution. The bootstrap test 
    ## p-value is the proportion of bootstrapped test statistics that are at least as big 
    ## as the observed one.
    p_value_par[i] <- 1 - pf(T2_0_scaled, p, n - p)
    p_value_boot[i] <- mean(T2_b_scaled >= T2_0_scaled, na.rm = TRUE)
  }

  return(list("p_values_par" = p_value_par, "p_values_boot" = p_value_boot))
}

## Type I error rates. Both methods are conservative in this example. Suggests that the 
## T2 test is robust to the multiariate t, but that's not a guaranty that it will be 
## robust in all situations.
H_0_out <- sim_f(mu = rep(0, p))
mean(H_0_out$p_values_par < 0.05)
mean(H_0_out$p_values_boot < 0.05)

## Type II error rates when mu' = [-0.5, 0, 0.5]. The bootstrap has very low power in 
## this example.
H_a_out <- sim_f(mu = c(-0.5, 0, 0.5))
mean(H_a_out$p_values_par < 0.05)
mean(H_a_out$p_values_boot < 0.05)

## Still, the bootstrap is available generally when the null sampling distribution of the 
## test statistic is unknown. Here, the sampling distribution is not F.
par(mfrow = c(1, 1))
hist(T2_b_scaled, xlab = expression("Scaled" ~ T^2), main = "")
qqplot(qf(ppoints(B), p, n - p), T2_b_scaled, xlab = "Theoretical Percentiles", 
  ylab = "Sample Percentiles", main = expression("QQ Plot for" ~ F[p, n-p]))
qqline(T2_b_scaled, distribution = function(p) qf(p, p, n - p), prob = c(0.1, 0.6))




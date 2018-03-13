####
#### Perspiration from 20 healthy females was analyzed. Three components, X_1 = sweat 
#### rate, X_2 = sodium content, and X_3 = potassium content, were measured.
####

## Load data.
X <- read.table("T5-1.DAT", header = FALSE)
colnames(X) <- c("Sweat", "Sodium", "Potassium")
attach(X)

n <- nrow(X)
p <- 3

## Summary statistics.
x_bar <- colMeans(X)
S <- var(X)

##
## Check normality.
##

pdf("figures/sweat_QQ.pdf", width = 7, height = 3.5)
par(mfrow = c(1, 3))
qqnorm(Sweat, main = "Sweat"); qqline(Sweat)
qqnorm(Sodium, main = "Sodium"); qqline(Sodium)
qqnorm(Potassium, main = "Potassium"); qqline(Potassium)
dev.off()

pdf("figures/sweat_pairs.pdf")
pairs(X)
dev.off()

##
## 95% confidence ellipse.
##

## The ellipse is centered at the sample mean vector, with axes equal to the eigenvectors 
## of S.
ee <- eigen(S)
lambda <- ee$values
ee <- ee$vectors

## The scaled F percentile that defines the desired squared distance.
scaled_F <- ((p * (n - 1)) / (n - p)) * qf(0.95, p, n - p)

## The half-lengths of the ellipse.
sqrt((lambda / n) * scaled_F)

##
## 95% T^2 simultaneous confidence intervals for the mean components.
##

x_bar[1] + c(-1, 1) * sqrt(scaled_F) * sqrt(S[1, 1] / n)
x_bar[2] + c(-1, 1) * sqrt(scaled_F) * sqrt(S[2, 2] / n)
x_bar[3] + c(-1, 1) * sqrt(scaled_F) * sqrt(S[3, 3] / n)

##
## 95% Bonferroni simultaneous confidence intervals for the mean components.
##

x_bar[1] + c(-1, 1) * qt(1 - 0.05 / (2 * p), n - 1) * sqrt(S[1, 1] / n)
x_bar[2] + c(-1, 1) * qt(1 - 0.05 / (2 * p), n - 1) * sqrt(S[2, 2] / n)
x_bar[3] + c(-1, 1) * qt(1 - 0.05 / (2 * p), n - 1) * sqrt(S[3, 3] / n)

##
## Hotelling's T^2 test of H_0: mu' = [3.5, 54.5, 8.8]. This is equivalent to checking 
## whether mu_0 is inside the 95% confidence ellipse from above. It is, so, as expected, 
## we fail to reject H_0.
##

mu_0 <- c(4.0, 45.0, 10.0)
T2 <- n * t(x_bar - mu_0) %*% solve(S) %*% (x_bar - mu_0)
p_value <- 1 - pf(((n - p) / (p * (n - 1))) * T2, p, n - p)

##
## Bootstrap test, using Lambda = (|S| / |S_0|)^{n/2} as the test statistic. Under the 
## assumption of multivariate normality, this equals the likelihood ratio statistic. If 
## n were "big", and assuming we were happy assuming multivariate normality, we could 
## compute a p-value via the large-sample chi-square result for likelihood ratio tests. 
## However, we can *always* use the bootstrap, even if n is small and normality does not 
## hold. 
##

set.seed(101)

## Our observed value of Lambda.
S_0_f <- function(X, mu_0) {
  matrix(rowSums(apply(X, 1, function(x) { (x - mu_0) %*% t(x - mu_0) })), nrow = p) / 
    (n - 1)
}

Lambda <- (det(S) / det(S_0_f(X, mu_0))) ^ (n / 2)

B <- 500
Lambda_b <- rep(NA, B)
X_0 <- scale(X, center = TRUE, scale = FALSE) + matrix(rep(mu_0, each = n), nrow = n)
for(b in 1:B) {
  cat(".")

  ## Create bootstrap sample.
  X_b <- X_0[sample(1:n, replace = TRUE), ]

  ## Compute Lambda.
  S_b <- var(X_b)
  S_0_b <- S_0_f(X_b, mu_0)
  Lambda_b[b] <- (det(S_b) / det(S_0_b)) ^ (n / 2)
}

## We again fail to reject H_0.
p_value_boot <- mean(Lambda_b <= Lambda)


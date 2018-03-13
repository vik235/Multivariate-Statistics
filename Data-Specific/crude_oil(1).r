####
#### Measurements on n = 56 crude oil samples from three stratigraphic populations: 
#### Willhelm, Sub-Mulinia, and Upper sandstone. We have p = 5 numerical variables, 
#### representing vanadium (in percent ash), iron (in percent ash), beryllium (in percent 
#### ash), saturated hydrocarbons (in percent ash), and aromatic hydrocarbons (in percent 
#### ash).
####

dta <- read.csv("crude_oil.csv")
Y <- dta$V6
X <- dta[, 1:5]
n <- nrow(X)
p <- ncol(X)
n_k <- table(Y)

## Transformations suggested by original authors.
X[, 2] <- sqrt(X[, 2])
X[, 3] <- sqrt(X[, 3])
X[, 4] <- 1 / X[, 4]

## Summary statistics.
X_bar <- by(X, Y, colMeans)
x_bar_0 <- colMeans(X)
S <- by(X, Y, var)
W <- (n_k[1] - 1) * S[[1]] + (n_k[2] - 1) * S[[2]] + (n_k[3] - 1) * S[[3]]
B <- n_k[1] * (X_bar[[1]] - x_bar_0) %*% t(X_bar[[1]] - x_bar_0) + 
  n_k[2] * (X_bar[[2]] - x_bar_0) %*% t(X_bar[[2]] - x_bar_0) + 
  n_k[3] * (X_bar[[3]] - x_bar_0) %*% t(X_bar[[3]] - x_bar_0)

## Linear discriminants.
library(MASS)

lda_out <- lda(X, Y, method = "mle")
lda_pred <- predict(lda_out, newdata = X)
LDs <- lda_pred$x

plot(LDs[, 1], LDs[, 2], xlab = "LD1", ylab = "LD2")
points(LDs[Y == "SubMuli", 1], LDs[Y == "SubMuli", 2], col = "blue", pch = 20)
points(LDs[Y == "Upper", 1], LDs[Y == "Upper", 2], col = "red", pch = 20)
points(LDs[Y == "Wilhelm", 1], LDs[Y == "Wilhelm", 2], col = "green", pch = 20)

Y_hat <- lda_pred$class
conf_mat <- table(Y, Y_hat)

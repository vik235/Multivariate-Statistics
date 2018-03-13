## Load data.
dta <- read.csv('F:/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/happiness.csv' , header = TRUE)
#dta <- read.csv("happiness.csv")
colnames(dta) <- c("Country", "Region", "Religion", "Religious", "Economy",
                   "Family", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
dim(dta)
summary(dta)
n <- nrow(dta)

#Just work with the data needed
happiness = dta[ , c( "Economy" , "Family" , "Health" , "Freedom" , "Trust" , "Generosity" , "Dystopia"
)]

fit <- lm(Dystopia ~ Economy + Family + Health + Freedom + Trust + Generosity,
          data = dta)
summary(fit)

## Fitted values.
y_hat <- predict(fit, newdata = dta)
plot(dta$Dystopia, y_hat)
?abline(0, 1, lty = 2)
lines(lowess(dta$Dystopia, y_hat))
2
## Cross validation (LOO) to estimate MSE.
set.seed(1)
MSE_cv_i <- numeric(n)
for(i in 1:n) {
  dta_te <- dta[i, , drop = FALSE]
  dta_tr <- dta[-i, ]
  fit_cv <- lm(Dystopia ~ Economy + Family + Health + Freedom + Trust + Generosity,
               data = dta_tr)
  y_hat_cv <- predict(fit_cv, newdata = dta_te)
  MSE_cv_i[i] <- (y_hat_cv - dta_te$Dystopia)^2
}
MSE_cv <- mean(MSE_cv_i)

B <- 1000
MSE_b <- numeric(B)
for(b in 1:B) {
  dta_b <- dta[sample(1:n, replace = TRUE), ]
  fit_b <- lm(Dystopia ~ Economy + Family + Health + Freedom + Trust + Generosity,
              data = dta_b)
  y_hat_b <- predict(fit_b, newdata = dta_b)
  MSE_b[b] <- mean((y_hat_b - dta_b$Dystopia)^2)
}
SD_MSE <- sd(MSE_b)

distr = MSE_distr_bootstr(happiness , "Dystopia" , 1000)
sd(distr)
head(happiness)

library("glmnet")
## Set up data in (Y, X) form.
Y <- dta$Dystopia
X <- as.matrix(dta[, c("Economy", "Family", "Health", "Freedom", "Trust",
                       "Generosity")])
## CV to choose optimal tuning parameter value. By default, the 'cv.glmnet' function
## considered 'lambda' values ranging from 0 (no shrinkage, equivalent to a usual
## least squares fit to the model) to 0.072, finding that the optimal lambda (the
## lambda that corresponds to the smallest CV-based MSE) is the maximum considered
## value, 0.072, and this threshold results in all predictor variables being
## shrunken out of the model.
fit_lasso_cv <- cv.glmnet(X, Y, family = "gaussian")
plot(fit_lasso_cv)
fit_lasso_cv$lambda
fit_lasso_cv$cvm
3
fit_lasso_cv$lambda.min
fit_lasso_cv$nzero
## Now fit the tuned version of our model (the one using lambda = 0.072) to all the
## training data.
fit_lasso <- glmnet(X, Y, lambda = fit_lasso_cv$lambda, family = "gaussian")
fit_lasso$beta[, 1]
y_hat_lasso <- predict(fit_lasso, newx = X, s = fit_lasso_cv$lambda.min)


MSE_lasso_cv_i <- numeric(n)
for(i in 1:n) {
  Y_te <- Y[i]
  X_te <- X[i, , drop = FALSE]
  Y_tr <- Y[-i]
  X_tr <- X[-i, ]
  fit_lasso_cv_1 <- glmnet(X_tr, Y_tr, lambda = fit_lasso_cv$lambda,
                           family = "gaussian")
  y_hat_lasso_cv_1 <- predict(fit_lasso_cv_1, newx = X_te,
                              s = fit_lasso_cv_1$lambda.min)[1]
  MSE_lasso_cv_i[i] <- (y_hat_lasso_cv_1 - Y_te)^2
}
MSE_lasso_cv <- mean(MSE_lasso_cv_i)


## Bootstrap to estimate sd of MSE estimate.
B <- 1000
MSE_b <- numeric(B)
for(b in 1:B) {
  ii_b <- sample(1:n, replace = TRUE)
  Y_b <- Y[ii_b]
  X_b <- X[ii_b, ]
  fit_lasso_boot <- glmnet(X_b, Y_b, lambda = fit_lasso_cv$lambda,
                           family = "gaussian")
  y_hat_lasso_boot <- predict(fit_lasso_boot, newx = X_b,
                              s = fit_lasso_boot$lambda.min)[, 1]
  MSE_b[b] <- mean((y_hat_lasso_boot - Y_b) ^ 2)
}
SD_MSE_lasso <- sd(MSE_b)

####
#### Toy example.
####

y_1 <- c(1, 4, 3, 8, 9)
y_2 <- c(-1, -1, 2, 3, 2)
x <- 0:4

## Scatterplots.
par(mfrow = c(1, 2))
plot(x, y_1)
plot(x, y_2)

## Univariate regressions.
fit_univ_1 <- lm(y_1 ~ 1 + x)
fit_univ_2 <- lm(y_2 ~ 1 + x)
summary(fit_univ_1)
summary(fit_univ_2)

plot(x, y_1)
abline(fit_univ_1)
plot(x, y_2)
abline(fit_univ_2)

## Multivariate regression.
fit_mult <- lm(cbind(y_1, y_2) ~ 1 + x)
summary(fit_mult)
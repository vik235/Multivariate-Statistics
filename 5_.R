X = read.csv("peanut.csv", header = T)
n <- 2
p <- 3
g <- 2
b <- 3
X$Location = as.factor(X$Location)
X$Variety = as.factor(X$Variety)
attach(X)
colnames(X) <- c("Factor_1", "Factor_2", "X_1", "X_2", "X_3")
attach(X)

x_bar= apply(X[,3:5], 2, mean )


x_bar_l_dot = rbind(apply(X[X$Factor_1 == "1",3:5], 2, mean),
                   apply(X[X$Factor_1 == "2",3:5], 2, mean) )
x_bar_dot_k = rbind(apply(X[X$Factor_2 == "5",3:5], 2, mean) ,
                   apply(X[X$Factor_2 == "6",3:5], 2, mean) ,
                   apply(X[X$Factor_2 == "8",3:5], 2, mean) )

x_bar_lk = rbind(apply(X[X$Factor_1 == "1" &  X$Factor_2 == "5",3:5], 2, mean) ,
                 apply(X[X$Factor_1 == "1" &  X$Factor_2 == "6",3:5], 2, mean) ,
                 apply(X[X$Factor_1 == "1" &  X$Factor_2 == "8",3:5], 2, mean) ,
                 apply(X[X$Factor_1 == "2" &  X$Factor_2 == "5",3:5], 2, mean) ,
                 apply(X[X$Factor_1 == "2" &  X$Factor_2 == "6",3:5], 2, mean) ,
                 apply(X[X$Factor_1 == "2" &  X$Factor_2 == "8",3:5], 2, mean) 
)


## Components for MANOVA.
SSP_cor <- SSP_fac_1 <- SSP_fac_2 <- SSP_int <- SSP_res <- matrix(0, nrow = p, ncol = p)
for(l in 1:g) {
  SSP_fac_1 <- SSP_fac_1 + b * n * t(x_bar_l_dot[l, , drop = FALSE] - x_bar) %*% 
    (x_bar_l_dot[l, , drop = FALSE] - x_bar)
  SSP_fac_2 <- SSP_fac_2 + g * n * t(x_bar_dot_k[l, , drop = FALSE] - x_bar) %*% 
    (x_bar_dot_k[l, , drop = FALSE] - x_bar)
  for(k in 1:b) {
    SSP_int <- SSP_int + n * t(x_bar_lk[(l - 1) * 2 + k, , drop = FALSE] - 
                                 x_bar_l_dot[l, , drop = FALSE] - x_bar_dot_k[k, , drop = FALSE] + x_bar) %*% 
      (x_bar_lk[(l - 1) * 2 + k, , drop = FALSE] - x_bar_l_dot[l, , drop = FALSE] - 
         x_bar_dot_k[k, , drop = FALSE] + x_bar)
    for(r in 1:n) {
      SSP_res <- SSP_res + t(as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
                               x_bar_lk[(l - 1) * 2 + k, , drop = FALSE]) %*% 
        (as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
           x_bar_lk[(l - 1) * 2 + k, , drop = FALSE])
      SSP_cor <- SSP_cor + t(as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
                               x_bar) %*% (as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - x_bar)
    }
  }
}

for(l in 1:g) {
  SSP_fac_1 <- SSP_fac_1 + b * n * t(x_bar_l_dot[l, , drop = FALSE] - x_bar) %*% 
    (x_bar_l_dot[l, , drop = FALSE] - x_bar)
}


WilksLamdba = det(SSP_res)/(det(SSP_fac_1 + SSP_res))

-2*log(WilksLamdba)

summary(manova(as.matrix(X[, 3:5]) ~ Factor_1 + Factor_2 + Factor_1 * Factor_2), 
        test = "Wilks")

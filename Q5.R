X = read.csv("C:/Users/vigupta/OneDrive/<>", header = T)
X$Location = as.factor(X$Location)
X$Variety = as.factor(X$Variety)
attach(X)
Y = cbind(X$X_1, X$X_2 , X$X_3)
t = manova(Y ~ Location + Variety + Location*Variety )
summary(t , test = 'Wilks')


summary(manova(as.matrix(X[, 3:5]) ~ Location + Variety + Location * Variety), 
        test = "Wilks")

g = length(unique(X$Location))
b = length(unique(X$Variety))
n = 2
p = 3

Xbar= apply(X[,3:5], 2, mean )


Xbar_l_dot = rbind(apply(X[X$Location == "1",3:5], 2, mean),
                   apply(X[X$Location == "2",3:5], 2, mean) )
Xbar_dot_k = rbind(apply(X[X$Variety == "5",3:5], 2, mean) ,
                   apply(X[X$Variety == "6",3:5], 2, mean) ,
                   apply(X[X$Variety == "8",3:5], 2, mean) )

Xbar_l_k = rbind(apply(X[X$Location == "1" &  X$Variety == "5",3:5], 2, mean) ,
                 apply(X[X$Location == "1" &  X$Variety == "6",3:5], 2, mean) ,
                 apply(X[X$Location == "1" &  X$Variety == "8",3:5], 2, mean) ,
                 apply(X[X$Location == "2" &  X$Variety == "5",3:5], 2, mean) ,
                 apply(X[X$Location == "2" &  X$Variety == "6",3:5], 2, mean) ,
                 apply(X[X$Location == "2" &  X$Variety == "8",3:5], 2, mean) 
                  )
SSLocation <- SSVariety <- SSInteraction <- SSP_res <- SSP_corr <- matrix(rep(0,p^2), nrow = p, byrow = TRUE )

#SSLocation
for (l in 1:g) {
  SSLocation <- SSLocation + b*n*(Xbar_l_dot[l,] - Xbar)%*%t(Xbar_l_dot[l,] - Xbar)
}

#SSVariety
for (k in 1:b) {
  SSVariety <- SSVariety + g*n*(Xbar_dot_k[k,] - Xbar)%*%t(Xbar_dot_k[k,] - Xbar)
}

#SSP_res
for (l in 1:g) {
  for (k in 1:b) {
    for (r in 1:n) {
      SSP_res <- SSP_res + t(as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
                               Xbar_l_k[(l - 1) * 2 + k, , drop = FALSE]) %*% 
        (as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
           Xbar_l_k[(l - 1) * 2 + k, , drop = FALSE])
      
          }
    
  }
}

#SSInteraction
for (l in 1:g) {
  for (k in 1:b) {
    SSInteraction <- SSInteraction + n * t(Xbar_l_k[(l - 1) * 2 + k, , drop = FALSE] - 
                                             Xbar_l_dot[l, , drop = FALSE] - Xbar_dot_k[k, , drop = FALSE] + Xbar) %*% 
      (Xbar_l_k[(l - 1) * 2 + k, , drop = FALSE] - Xbar_l_dot[l, , drop = FALSE] - 
         Xbar_dot_k[k, , drop = FALSE] + Xbar)
    
  }
  
}


##SSP_Corrected

for (l in 1:g) {
  for (k in 1:b) {
    for (r in 1:n) {
      SSP_corr <- SSP_corr + t(as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - 
                               Xbar) %*% (as.matrix(X[(l - 1) * 2 * n + (k - 1) * n + r, 3:5]) - Xbar)
      
    }
    
  }
}

WilksLamdba = det(SSP_res1)/(det(SSLocation + SSP_res1))

(X[3:5] - Xbar)

SSP_res1 = matrix(rep(0,p^2), nrow = p, byrow = TRUE )
for (s in 1:nrow(X)) {
  SSP_res1 = SSP_res1 + t(as.matrix(X[s,3:5] - Xbar , drop = T))%*%(as.matrix(X[s,3:5] - Xbar , drop = T))
}

det(SSP_res1)

as.matrix(X[1,3:5] - Xbar)%*%t(as.matrix(X[1,3:5] - Xbar))

s= 2
t(as.matrix(X[s,3:5] - Xbar , drop = T))%*%(as.matrix(X[s,3:5] - Xbar , drop = T))
?t

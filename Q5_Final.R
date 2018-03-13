###
###Vivek K Gupta, Simulating calculations for 2 way Manova on the peanut data
###Date: 10/05/17
###
X = read.csv("peanut.csv", header = T)
X$Location = as.factor(X$Location)
X$Variety = as.factor(X$Variety)
colnames(X) <- c("Factor_1", "Factor_2", "X_1", "X_2", "X_3")
attach(X)
g = length(unique(X$Factor_1))
b = length(unique(X$Factor_2))
n = 2
p = 3

par(mfrow = c(1, 3))
boxplot(X[,3]~ Factor_1*Factor_2, names = c("1-5","2-5","1-6","2-6","1-8","2-8"), xlab="Location-Variety Combination"
        ,ylab="Weight of Total Yield")

boxplot(X[,4]~ Factor_1*Factor_2, names = c("1-5","2-5","1-6","2-6","1-8","2-8"), xlab="Location-Variety Combination"
        ,ylab="Mature Kernel")


boxplot(X[,5]~ Factor_1*Factor_2, names = c("1-5","2-5","1-6","2-6","1-8","2-8"), xlab="Location-Variety Combination"
        ,ylab="Seed Size")
dev.off()
x_bar= apply(X[,3:5], 2, mean )

## Summary statistics.
x_bar <- colMeans(X[, 3:5])
x_bar_lk <- as.data.frame(rbind(c(Factor_1 =1, Factor_2= 5, colMeans(X[Factor_1 == 1 & Factor_2 == 5, 3:5])), 
                  c(Factor_1 =1, Factor_2= 6, colMeans(X[Factor_1 == 1 & Factor_2 == 6, 3:5])), 
                  c(Factor_1 =1, Factor_2= 8, colMeans(X[Factor_1 == 1 & Factor_2 == 8, 3:5])), 
                  c(Factor_1 =2, Factor_2= 5, colMeans(X[Factor_1 == 2 & Factor_2 == 5, 3:5])), 
                  c(Factor_1 =2, Factor_2= 6, colMeans(X[Factor_1 == 2 & Factor_2 == 6, 3:5])), 
                  c(Factor_1 =2, Factor_2= 8, colMeans(X[Factor_1 == 2 & Factor_2 == 8, 3:5]))))
x_bar_lk$Factor_1 = as.factor(x_bar_lk$Factor_1)
x_bar_lk$Factor_2 = as.factor(x_bar_lk$Factor_2)

x_bar_l_dot <- as.data.frame(rbind(c(Factor_1 =1, colMeans(X[Factor_1 == 1, 3:5])), 
                     c(Factor_1 =2,colMeans(X[Factor_1 == 2, 3:5]))))
x_bar_l_dot$Factor_1 = as.factor(x_bar_l_dot$Factor_1)

x_bar_dot_k <- as.data.frame(rbind(c(Factor_2 =5,colMeans(X[Factor_2 == 5, 3:5])), 
                     c(Factor_2 =6,colMeans(X[Factor_2 == 6, 3:5])),
                     c(Factor_2 =8,colMeans(X[Factor_2 == 8, 3:5]))))
x_bar_dot_k$Factor_2 = as.factor(x_bar_dot_k$Factor_2)

E <-  matrix(0, nrow = p, ncol = p)
H_fac_1 <- matrix(0, nrow = p, ncol = p)
H_fac_2 <- matrix(0, nrow = p, ncol = p)
T_Corr <- matrix(0, nrow = p, ncol = p)
Hab <- matrix(0, nrow = p, ncol = p)

#E calc
for (l in g_levels) {
  for (k in b_levels) {
    for (r in 1:n) {
      Op = as.matrix(X[X$Factor_1 == l & X$Factor_2 == k,3:5][r,] - x_bar_lk[x_bar_lk$Factor_1 == l & x_bar_lk$Factor_2 == k,3:5])
      E = E + t(Op)%*%(Op)
      
    }
    
  }
}

#H for Factor 1

for (l in g_levels) {
  Op = as.matrix(x_bar_l_dot[x_bar_l_dot$Factor_1 == l, -1] - x_bar)
  H_fac_1 = H_fac_1 + n*b*t(Op)%*%(Op)
}

#H for Factor 2

for (k in b_levels) {
  Op = as.matrix(x_bar_dot_k[x_bar_dot_k$Factor_2 == k, -1] - x_bar)
  H_fac_2 = H_fac_2 + n*g*t(Op)%*%(Op)
}

#T calc
for (l in g_levels) {
  for (k in b_levels) {
    for (r in 1:n) {
      Op = as.matrix(X[X$Factor_1 == l & X$Factor_2 == k,3:5][r,] - x_bar)
      T_Corr = T_Corr + t(Op)%*%(Op)
      
    }
    
  }
}

#Hab calc
for (l in g_levels) {
  for (k in b_levels) {
  
      Op = as.matrix(x_bar_lk[x_bar_lk$Factor_1 == l & x_bar_lk$Factor_2 == k,3:5] -
            x_bar_l_dot[x_bar_l_dot$Factor_1 == l, -1] -
            x_bar_dot_k[x_bar_dot_k$Factor_2 == k, -1] +
            x_bar)
      Hab = Hab + n*t(Op)%*%(Op)
  }
}
#
#H_fac_1
#

#X_1       X_2         X_3
#X_1   0.7008333  -10.6575    7.129167
#X_2 -10.6575000  162.0675 -108.412500
#X_3   7.1291667 -108.4125   72.520833

#
#H_fac_2
#

#X_1       X_2      X_3
#X_1 196.1150  365.1825  42.6275
#X_2 365.1825 1089.0150 414.6550
#X_3  42.6275  414.6550 284.1017

#
#Hab
#
#X_1      X_2       X_3
#X_1 205.1017 363.6675 107.78583
#X_2 363.6675 780.6950 254.22000
#X_3 107.7858 254.2200  85.95167

#
#T_Corr
#

#X_1       X_2      X_3
#X_1 506.1225  767.5575 234.0225
#X_2 767.5575 2383.8825 682.4575
#X_3 234.0225  682.4575 537.4092

#Interaction 

Lambda = det(E)/det(E + Hab)
#Wilks for Interaction = 0.07429984
# p value 
1 - pf(((1 - sqrt(Lambda))/(sqrt(Lambda)))*(6 - p + 1)/p , 2*p, 2*(6 - p + 1))
#0.05079425 
#Conclusion : At alpha= .05 , we have small evidence to conclude that significant interaction does not exist
#between the 2 factors


#Effect of Factor 1 

Lambda = det(E)/det(E + H_fac_1)
#Wilks for Interaction = 0.1065162
# p value 
1 - pf(((1 - (Lambda))/((Lambda)))*(6 - p + 1)/p , p, (6 - p + 1))
#0.02050211 
#Conclusion : At alpha= .05 , we have evidence at p value = 0.02050211 to conclude that the difference does exist
#due to Factor 1 


#Effect of Factor 2 

Lambda = det(E)/det(E + H_fac_2)
#Wilks for Interaction = 0.01244417
# p value 
1 - pf(((1 - sqrt(Lambda))/(sqrt(Lambda)))*(6 - p + 1)/p , 2*p, 2*(6 - p + 1))
#0.001927533 
#Conclusion : At alpha= .05 , we have strong evidence at p value = 0.001927533 to conclude that the difference exist
#due to Factor 2 

#Manova calculations via inbuilt function
summary(manova(cbind(X_1,X_2,X_3) ~ Factor_1 + Factor_2 + Factor_1*Factor_2), test= 'Wilks')

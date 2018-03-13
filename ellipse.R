##Vivek Kumar Gupta, 9/9/2017. Purpose, drawing ellipses and a constant mahalanobis distance from mu. 
##Simulating bvtnormals and fidnign proportions of observations inside the ellipse.

#Set the constant stat distance, note Mahalanobis dist(i.e cSquared) ~ chisq(p)
c=sqrt(qchisq(.95,2))

#population mean vector. CHANGE this if needed
mu=c(1,1)

#Population dispersion matric. CHANGE this if needed
#sigma_ec12 = matrix(c(1,.80,.80,1), nrow = 2, byrow = T)
#sigma_ec12 = matrix(c(1,0,0,1), nrow = 2, byrow = T)
#sigma_ec12 = matrix(c(1,-.80,-.80,1), nrow = 2, byrow = T)
#sigma_ec12 = matrix(c(1,.40,.40,.25), nrow = 2, byrow = T)
#sigma_ec12 = matrix(c(1,0,0,.25), nrow = 2, byrow = T)
#sigma_ec12 = matrix(c(1,-.40,-.40,.25), nrow = 2, byrow = T)
#sigma_ec12 = matrix(c(.25,.40,.40,1), nrow = 2, byrow = T)
#sigma_ec12 = matrix(c(.25,0,0,1), nrow = 2, byrow = T)
sigma_ec12 = matrix(c(.25,-.40,-.40,1), nrow = 2, byrow = T)

#Form a sigma_inverse matrix
sigma_ec1=solve(sigma_ec12)

#Decompose inverse of cov matrix into eigens and transform eignevectors to a dataframe 
#for easier manipulation- this can be skipped
eigen_ec1=eigen(sigma_ec1)
eigenvectors=data.frame(eigen_ec1$vectors)

#Compute a and b's of the ellipse i.e the radii, a > b
half_l_ec1= c/sqrt(eigen_ec1$values[1])
half_s_ec1= c/sqrt(eigen_ec1$values[2])

#Getting Rho matrix, not needed but for reference
D=diag(sqrt(diag(sigma_ec1)),2)
Rho = solve(D)%*%sigma_ec1%*%solve(D)

#Calculate the angle in radians for the rotation. 
#Note its calculated from first eigen vector
theta=atan((eigenvectors$X1[2])/(eigenvectors$X1[1]))
#check (180/pi)*theta

#Plotrix for draw.ellipse function and mvtnorm for simulating bvtnorms.

library(plotrix)
library(mvtnorm)


Y=rmvnorm(5000,mean=c(1,1),sigma = sigma_ec12)

plot(Y, xlim = c(-8,8), ylim = c(-8,8), main = "Ellipse 9")

draw.ellipse(1,1,half_l_ec1,half_s_ec1, angle = theta, deg = F, 
             border = "red", lwd = 2)
abline(h=1,lty=2,col=2)
abline(v=1,lty=2,col=2)
abline(h=0,lty=2 , col=4)
abline(v=0,lty=2, col=4)

## Y is a matrix of bvtrnorms, mu is our popn mean, sigma_ec1 is cov matrix
temp=(Y - mu)%*%solve(sigma_ec12)
center=Y-mu

#these two numbers should ideally match
(prop_inside = sum(((temp[,1]*(center[,1]) +temp[,2]*(center[,2]))) <=c^2)/5000)
(sum(mahalanobis(Y,mu,sigma_ec12, inverted = F) <= c^2)/5000)

plot(density(mahalanobis(Y,mu,sigma_ec12, inverted = F)))
plot(density(rchisq(500,2)))

a= seq(-2,4,length=50)
b= seq(-1,5, length=50)     
c=seq(0,6, length=50)     
Y= data.frame(a,b,c)
mu = c(1,2,3)
Sigma=matrix(c(2,.4,.6,.4,3,.8,.6,.8,4), nrow = 3, byrow = T)

plot(density(mahalanobis(Y,mu,Sigma, inverted = F)))
plot(density(rchisq(500,3)))

X <- matrix(c(3,4,5,4,6,4,7,7), ncol=2)
p= ncol(X)
n = nrow(X)
##Calculationg MLE for mu.
mu_mle = apply(X,2,mean)
#Answers
#mu_mle = t(4,6)
#that is , the MLE estimate of the mu vector of the data is (4,6)'. 

##Calculating MLE for Sigma.

I= diag(1, n)
J= matrix(rep(1, n^2), nrow=n, byrow = TRUE)
S_n = 1/n*(t(X)%*%( I - (1/n)*J)%*%X) # Remember MLE_S= (n*S_n)/(n-1)


#MLE estimate of Sigma is 

#     [,1] [,2]
#[1,] 0.50 0.25
#[2,] 0.25 1.50



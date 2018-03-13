####                ...
####Vivek Kumar Gupta, vivek235@tamu.edu , Stats 636 Exam 1 Code Work. 
####Date:10/11/2017
####Purpose: Response in R to the test questions. Test by Dr. Dabney TAMU, Statistics. 
####
####

##****
##Question 1 
##****


#Setup
library(MASS)
N <- 1000
n <- 50
p <- 5
mu <- rep(0, p)
rho <- 0.8
Rho <- matrix(rho, nrow = p, ncol = p); diag(Rho) <- 1
sg <- 1
Sigma <- sg * Rho


#Setup arrays needed for Upper Conf bound, lower conf bound and whether 
#an iteration covers the true mean of a variable 
#We will call variables as V1, V2, V3, V4, V5

UCI_v1 <- UCI_v2 <- UCI_v3 <- UCI_v4 <- UCI_v5 <-  rep(NA,N)
Cover_v1 <- Cover_v2 <- Cover_v3 <- Cover_v4 <- Cover_v5 <-  rep(0,N)
LCI_v1 <- LCI_v2 <- LCI_v3 <- LCI_v4 <- LCI_v5 <-  rep(NA,N)

for (i in 1:N) {
  
  X = mvrnorm(n , mu , Sigma)
  x_i_bar = apply(X, 2, mean)
  S_i = cov(X)
  
  #Standard error vector, adjusted for Bonferroni 
  SE_i = qt(1 - .05/(2*p), n - 1)*sqrt(diag(S_i)/n)
  
  #Calculate the CI for each variable in the iteration
  #Check if the UCI and LCI contains 0, if yes set Coverage of the variable to 1
  #Coverage here means whether the simulation contains the true mean or not.
  
  
  UCI_v1[i] = x_i_bar[1] + SE_i[1]
  LCI_v1[i] = x_i_bar[1] - SE_i[1]
  if (LCI_v1[i] < 0 & (UCI_v1[i] > 0) )
  {
    Cover_v1[i] = 1
  }
  
  UCI_v2[i] = x_i_bar[2] + SE_i[2]
  LCI_v2[i] = x_i_bar[2] - SE_i[2]
  
  if (LCI_v2[i] < 0 & (UCI_v2[i] > 0) )
  {
    Cover_v2[i] = 1
  }
  
  UCI_v3[i] = x_i_bar[3] + SE_i[3]
  LCI_v3[i] = x_i_bar[3] - SE_i[3]
  
  if (LCI_v3[i] < 0 & (UCI_v3[i] > 0) )
  {
    Cover_v3[i] = 1
  }
  
  UCI_v4[i] = x_i_bar[4] + SE_i[4]
  LCI_v4[i] = x_i_bar[4] - SE_i[4]
  
  if (LCI_v4[i] < 0 & (UCI_v4[i] > 0) )
  {
    Cover_v4[i] = 1
  }
  
  UCI_v5[i] = x_i_bar[5] + SE_i[5]
  LCI_v5[i] = x_i_bar[5] - SE_i[5]
  if (LCI_v5[i] < 0 & (UCI_v5[i] > 0) )
  {
    Cover_v5[i] = 1
  }
  
}

#Overall Coverage probabilities V1
100*mean(Cover_v1)
#99.5

#Overall Coverage probabilities V2
100*mean(Cover_v2)
#99.3

#Overall Coverage probabilities V3
100*mean(Cover_v3)

#Overall Coverage probabilities V4
100*mean(Cover_v4)
#98.89

#Overall Coverage probabilities V5
100*mean(Cover_v5)
#99.6



##****
##Question 2
##****

# Loading the dataset and setting up 

#Read in the data set into R
oil <- read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/crude_oil.csv', header = T)

#Alter the col names to more user friendly ones
colnames(oil) <- c('X_1','X_2','X_3','X_4','X_5','Factor_1')

X = oil[,1:5] 

x_bar = apply(X, 2, mean)
S = var(X)
n = nrow(X) 
p = 5 
nu = n - 1 

## PART a
##95% Confidence Region : This is given by a hyperellipsoid centered at "center", with half lenghts "half_lengths" 
##and axes given by "eigens$vectors" as shown below
##
c = sqrt((nu*p/(nu - p + 1))*qf(1 - .05 , p , (nu - p + 1)))

eigens = eigen(S)

center = x_bar
#    X_1        X_2        X_3        X_4        X_5 
#6.1803571 27.0464286  0.3414286  5.2991071  6.4335714 

half_lengths = sqrt((eigens$values)/n) * (c)
#5.6177550 1.5022531 1.0715919 0.4659466 0.1396054

#primary axes, Note:  Axes_i' = eigens$vectors[,i], i = 1 to 5 
eigens$vectors

###########[,1]        [,2]         [,3]        [,4]         [,5]
#[1,]  0.095153893  0.02252492  0.957937396  0.26822347  0.029162981
#[2,] -0.992050278  0.06373259  0.104846646 -0.02713616 -0.006724052
#[3,]  0.004553247  0.01224669 -0.000430484  0.10698674 -0.994174501
#[4,] -0.063141164 -0.14285092 -0.258840368  0.94794174  0.100074656
#[5,] -0.052672993 -0.98735729  0.066065162 -0.13145379 -0.026578783



## PART b
##95% Simultaneous Bonferroni Intervals : These are calculated for each variables as below
##Note: alpha_pc = .05/2p , Upper percentile
##

#SE vector
se = qt(1 - .05/(2*p), n - 1)*sqrt(diag(S)/n)

#CI Variable X_1 
x_bar[1] + c(-1,1)* se[1]
#5.315959 7.044755


#CI Variable X_2
x_bar[2] + c(-1,1)* se[2]
#22.90842 31.18444


#CI Variable X_3
x_bar[3] + c(-1,1)* se[3]
#0.2294980 0.4533591

#CI Variable X_4
x_bar[4] + c(-1,1)* se[4]
#4.804479 5.793735

#CI Variable X_5
x_bar[5] + c(-1,1)* se[5]
#5.308798 7.558345

library(MVN)
library(alr3)
library(DescTools)
sweat <- read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/sweat.csv', header = TRUE)
head(sweat)

qqnorm(sweat$Sweat)
qqline(sweat$Sweat)
n = nrow(sweat)
p = 3 
nu = n - 1 
S = cov(sweat)

pairs(sweat)
nu = n - 1
x_bar = apply(sweat, 2, mean)

c = sqrt((nu*p/(nu - p + 1))*qf(1 - .05 , p , (nu - p + 1)))

eigen = eigen(S)

sqrt((eigen$values)/n) * (c)

eigen$vectors

#T2 simul
a = c(1 , 0 , 0)
se = c*sqrt((t(a)%*%S%*%a)/n)
t(a)%*%x_bar - se 
t(a)%*%x_bar + se 



a = c(0 , 1 , 0)
se = c*sqrt((t(a)%*%S%*%a)/n)
t(a)%*%x_bar - se 
t(a)%*%x_bar + se 


a = c(0 , 0 , 1)
se = c*sqrt((t(a)%*%S%*%a)/n)
t(a)%*%x_bar - se 
t(a)%*%x_bar + se 

#Bonfer
m = 3 
a = c(1 , 0 , 0)
se = qt(1 - .05/(2*m), n - 1)*sqrt(diag(S)/n)

#Sweat : 
t(a)%*%x_bar + as.vector(se)

x_bar[1] - se[1]
x_bar[1] + se[1]

mu_0 = c(4, 45, 10)

#T2 test 
stat = n* t(x_bar - mu_0)%*%solve(S)%*%(x_bar - mu_0)

pval = 1 - pf(stat * (nu - p + 1)/(nu*p) , p , (nu - p + 1))

TCrit = ((nu - p + 1)/(nu*p))^(-1) * qf(1 - .05 , p ,  (nu - p + 1))

#Assume independent trials. 
theta = .5 
n = 25000 
x = 2  #random variable of interest 

p = (theta^x)*(1 - theta)^(n - x)





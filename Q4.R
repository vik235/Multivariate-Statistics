library(MVN)
library(alr3)
sweat <- read.csv('sweat.csv', header = TRUE)
head(sweat)

n = nrow(sweat)
p= ncol(sweat)
nu= n-1
alpha=.05
c= sqrt((nu*p/(nu - p +1) )*qf(1 - alpha, p,nu - p +1 ))
plot(sweat)
##Solving for b
S = cov(sweat)

SInv= solve(S)
eigens=eigen(S)
eigenvectors=data.frame(eigens$vectors)
eigenvectors

(half_l_ec1= c*sqrt(eigens$values[1])/sqrt(n))
(half_s1_ec1= c*sqrt(eigens$values[2])/sqrt(n))
(half_s2_ec1= c*sqrt(eigens$values[3])/sqrt(n))

#center
Xbar= apply(sweat ,2 , mean)
#Center: (  4.640    45.400     9.965 )'

#Axis: 
#1- Major axis: (-0.05084144 -0.99828352  0.02907156)', half length: 46.35383
#2- Minor1 axis: (-0.57370364  0.05302042  0.81734508)', half length: 6.969385
#3- Minor2 axis: ( 0.81748351 -0.02487655  0.57541452)', half length: 3.734851

#Lengths
half_l_ec1= c*sqrt(eigens$values[1])
half_s1_ec1= c*sqrt(eigens$values[2])
half_s2_ec1= c*sqrt(eigens$values[3])

#directions. 
eigenvectors

##Solving for c 95% T2 confidence for the  Seat, Sodium and Potassium. 
#Sweat 
a = c(1,0,0)
taSa = t(a)%*%S%*%a
ci_Sweat= Xbar[1] +c(-1,1)*sqrt((c^2)*taSa/n)
#T2 simultaneous CI for Sweat
#3.397768 5.882232

#Sodium 
a = c(0,1,0)
taSa = t(a)%*%S%*%a
ci_Sodium= Xbar[2] +c(-1,1)*sqrt((c^2)*taSa/n)
#T2 simultaneous CI for Sodium
#44.15777 46.64223

#Potassium 
a = c(0,0,1)
taSa = t(a)%*%S%*%a
ci_Potassium= Xbar[3] +c(-1,1)*sqrt((c^2)*taSa/n)
#T2 simultaneous CI for Potassium
# 8.570664 11.359336

##Solving for d 95% Bonferroni confidence for the  Seat, Sodium and Potassium. 
m=3
alphapc=alpha/(2*m)
t = qt(1 - alphapc , n - 1)*sqrt(c(diag(S))/n)

#Sweat
Xbar[1] + c(-1,1)*t[1]
#3.643952 5.636048

#Sodium
Xbar[2] + c(-1,1)*t[2]
#37.10308 53.69692

#Potassium 
Xbar[3] + c(-1,1)*t[3]
#8.846992 11.083008


##Solving for e
mu0=c(4,45,10)
T2 = n*t(Xbar - mu0)%*%(SInv)%*%(Xbar - mu0)
#4.374632 = T2 test statistic. 

pvalue = 1 - pf(((nu - p +1)/(nu*p))*T2, p, nu - p +1 )
#0.3052847 P value of the test- Ho : mu=mu0

#T2_Critical 
((nu*p)/(nu - p +1))*qf(1 - 0.05, p, nu - p +1 )
#10.7186

# Conclusion: At a p value of 0.3052847 we dont have sufficient evidence (alpha=.05) 
# to reject the null hypothesis Ho : mu=mu0


#f

mu_1 = c(4, 45, 10)
MD = t(Xbar - mu_1)%*%SInv%*%(Xbar - mu_1)

#Compare MD with c 
MD < c
#TRUE
#Thus mu_1 is inside the 95% confidence region of mu which is consistent with what we concluded from part e.


#g
##Bootstrap setup. For testing via Generalized LR as its an optimal test.
#Tradeoff: n should be very large to approximate the dist. of ts to chi-sq. 
#In below case n is not that big so we piggy back to the bootstrap procedure. 

#Set the seed so that the results are repeatable. 
set.seed(101) 

#Transform the data matrix to force H0 to be true, one trick Xtilde= X - Xbar + mu0
sweat_t0 = sweat - matrix(rep(Xbar,n),nrow=n, byrow = TRUE) + matrix(rep(mu0,n),nrow=n, byrow = TRUE) 

#Setup the test statistic, refer to the problem. However note that we do not know the sampling distribution
#of this test statisic and hence to run the hypothesis test, we bootstrap to as shown below 
#to determine its distribution 
detS=det(var(sweat))
detS0= det(var(sweat_t0))
lambda_ts = (detS/detS0)^(n/2) #THE test statistic

#Bootstrap fromt he data under H0 to get the sampling dist.
B=500 
tb=rep(0,B)

for(i in 1:B)
{
 bS= sweat_t0[sample(1:n, replace = T),]
 tb[b] = (det(var(bS))/detS0)^(n/2)

}

#P value of the test 
p_value_boot <- mean(tb >= lambda)
#.306 

# We note that this is so close to the one obtained by the earlier procedures. 

install.packages("art")
install.packages("MVN")
library(MVN)
library(alr3)
car <- read.csv('used_cars.csv', header = TRUE)

#MV Transform to N
p1 <- powerTransform(car)
p1$roundlam[1]
car$AgeMVN=car$Age^p1$roundlam[1]
car$PriceMVN=log(car$Price)

#UV transform to N

y1 <- car$Price #sort(#data vector, +ve in real, example below)
  #y1 <- sort(rexp(100,80))
  n <- length(y1)
theta <- -3 #starting seed , we will start with this seed and go to the +ve value of the seed.
iterations <- seq(theta, abs(theta)*2, 0.001) # this holds theta's, power transforms 
yt0 <-log(y1)
var_yt0 <- var(yt0)
l0 <- (0-1)*sum(log(y1)) - 0.5*n*(log(2*pi*var_yt0)+1)
t0 <- 0
logLikelihood <- as.vector(rep(0,length(iterations)))# this holds logliklihoods 1:1 with power transforms 
for (i in 1:length(logLikelihood)) {
  
  yt <- (y1^iterations[i] - 1)/iterations[i]
  var_yt <- var(yt)
  logLikelihood[i] <- (iterations[i]-1)*sum(log(y1)) - 0.5*n*(log(2*pi*var_yt)+1)
  if(abs(iterations[i]) < 1.0e-10) iterations[i] <- 0 # to cover for the iteration value when theta->0 
  if(abs(iterations[i]) < 1.0e-10) logLikelihood[i] <-l0 # to cover for the iteration value when theta->0 
  
}
plot(iterations,logLikelihood)
(theta_max <- iterations[which(logLikelihood==max(logLikelihood))])
(tU = max(logLikelihood)+.5*qchisq(.95,1)) #Upper bound on theta
(tL=  max(logLikelihood)-.5*qchisq(.95,1))#lower bound on theta
(tM=max(logLikelihood)) #theta max

(iL <- min(which((logLikelihood > tL) & (logLikelihood < tM)))) #index of lower bound on theta 
(iU <- max(which((logLikelihood > tL) & (logLikelihood < tM))))#index of upper bound on theta 

abline(v=iterations[iL])
abline(v=iterations[iU])
abline(v=theta_max, col=2)


mardiaTest(car[,3:4], qqplot=TRUE)

mahalanobis(car[,3:4],mu,cov(car[,3:4]))<=qchisq(.5,2)
mu= c(mean(car$AgeMVN),mean(car$PriceMVN))
roystonTest(car[,3:4])
uniPlot(car[,3:4], type="histogram")
uniNorm(car[,3:4],type="SW", desc=T)

mvnO=roystonTest(car[,3:4])
mvnPlot(mvnO,type="contour")



#UV case 
p1_age <- powerTransform(car$Age)
p1_price <- powerTransform(car$Price)

p1_age$roundlam
p1_price$roundlam

qqplot(car$Age^(.37), rnorm(100))
shapiro.test(car$Price)

mvOutlier(car[,3:4])

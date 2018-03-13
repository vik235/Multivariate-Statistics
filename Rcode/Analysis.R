##
## Multivariate Analysis of Crude Oil data 
## Vivek Kumar Gupta, 10/11/2017
##

##
##Set all library calls
##

library("MVN")
library("MASS")
library("alr3")
#Read in the data set into R
oil <- read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/crude_oil.csv', header = T)

#Alter the col names to more user friendly ones
colnames(oil) <- c('X_1','X_2','X_3','X_4','X_5','Factor_1')

#See the structure 
str(oil)

#Create separate data matrices for  subgroups

oil.Wilhelm = oil[oil$Factor_1=='Wilhelm',]
oil.SubMuli = oil[oil$Factor_1=='SubMuli',]
oil.Upper = oil[oil$Factor_1=='Upper',]




#Verify MVN assumptions in each group 


#Wilhelm 
par(mfrow=c(3,3))
qqnorm(oil.Wilhelm$X_1 , main = "QQ plot of group Wilhelm,  X_1", ylab= "Quantiles of X_1")
qqline(oil.Wilhelm$X_1)

qqnorm(oil.Wilhelm$X_2 , main = "QQ plot of group Wilhelm,  X_2", ylab= "Quantiles of X_2")
qqline(oil.Wilhelm$X_2)

qqnorm(oil.Wilhelm$X_3 , main = "QQ plot of group Wilhelm,  X_3", ylab= "Quantiles of X_3")
qqline(oil.Wilhelm$X_3)

qqnorm(oil.Wilhelm$X_4 , main = "QQ plot of group Wilhelm,  X_4", ylab= "Quantiles of X_4")
qqline(oil.Wilhelm$X_4)

qqnorm(oil.Wilhelm$X_5 , main = "QQ plot of group Wilhelm,  X_5", ylab= "Quantiles of X_5")
qqline(oil.Wilhelm$X_5)
dev.off()

#SubMuli
par(mfrow=c(3,3))
qqnorm(oil.SubMuli$X_1 , main = "QQ plot of group SubMuli,  X_1", ylab= "Quantiles of X_1")
qqline(oil.SubMuli$X_1)

qqnorm(oil.SubMuli$X_2 , main = "QQ plot of group SubMuli,  X_2", ylab= "Quantiles of X_2")
qqline(oil.SubMuli$X_2)

qqnorm(oil.SubMuli$X_3 , main = "QQ plot of group SubMuli,  X_3", ylab= "Quantiles of X_3")
qqline(oil.SubMuli$X_3)

qqnorm(oil.SubMuli$X_4 , main = "QQ plot of group SubMuli,  X_4", ylab= "Quantiles of X_4")
qqline(oil.SubMuli$X_4)

qqnorm(oil.SubMuli$X_5 , main = "QQ plot of group SubMuli,  X_5", ylab= "Quantiles of X_5")
qqline(oil.SubMuli$X_5)
dev.off()

#Upper
par(mfrow=c(3,3))
qqnorm(oil.Upper$X_1 , main = "QQ plot of group Upper,  X_1", ylab= "Quantiles of X_1")
qqline(oil.Upper$X_1)

qqnorm(oil.Upper$X_2 , main = "QQ plot of group Upper,  X_2", ylab= "Quantiles of X_2")
qqline(oil.Upper$X_2)

qqnorm(oil.Upper$X_3 , main = "QQ plot of group Upper,  X_3", ylab= "Quantiles of X_3")
qqline(oil.Upper$X_3)

qqnorm(oil.Upper$X_4 , main = "QQ plot of group Upper,  X_4", ylab= "Quantiles of X_4")
qqline(oil.Upper$X_4)

qqnorm(oil.Upper$X_5 , main = "QQ plot of group Upper,  X_5", ylab= "Quantiles of X_5")
qqline(oil.Upper$X_5)
dev.off()
#Observation from QQ plot : The subgroups arent MVN.
#Setup Statistical tests. 

mardiaTest(oil.Wilhelm[,1:5], qqplot=TRUE)
mardiaTest(oil.SubMuli[,1:5], qqplot=TRUE)
mardiaTest(oil.Upper[,1:5], qqplot=TRUE)
#Mardia Test of skewness and Kurtosis confirms that atleast oil.Upper is not MVN

roystonTest(oil.Wilhelm[,1:5], qqplot=TRUE)
roystonTest(oil.SubMuli[,1:5], qqplot=TRUE)
roystonTest(oil.Upper[,1:5], qqplot=TRUE)
#Royston Test  confirms that atleast oil.Upperand Oil.SubMuli are not MVN

##
##Conclusion : Cannot guarantee MVN in subgroups
##

powerTransform(oil.Wilhelm[,3])

#Verify MVN assumptions in overall dataset 
par(mfrow=c(3,3))
qqnorm(oil$X_1 , main = "QQ plot of X_1", ylab= "Quantiles of X_1")
qqline(oil$X_1)

qqnorm(oil$X_2 , main = "QQ plot of X_2", ylab= "Quantiles of X_2")
qqline(oil$X_2)

qqnorm(oil$X_3 , main = "QQ plot of X_3", ylab= "Quantiles of X_3")
qqline(oil$X_3)

qqnorm(oil$X_4 , main = "QQ plot of X_4", ylab= "Quantiles of X_4")
qqline(oil$X_4)

qqnorm(oil$X_5 , main = "QQ plot of X_5", ylab= "Quantiles of X_5")
qqline(oil$X_5)



dev.off()

mardiaTest(oil[oil$,1:5], qqplot=TRUE)

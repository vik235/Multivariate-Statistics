require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
stat_class <- read.csv('F:/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/stat_class.csv' , header = TRUE)
stat_class <- na.omit(stat_class)
attach(stat_class)
head(stat_class)

table(stat_class$SEX, stat_class$GRADE)
tapply(HW, GRADE, function(x) c(M = mean(x), SD = sd(x)))
tapply(EXAM, GRADE, function(x) c(M = mean(x), SD = sd(x)))

head(stat_class)

stat_class$GRADE2 <- relevel(stat_class$GRADE, ref = "A")
relevel

fit.M1 <- ?multinom(GRADE ~ HW + EXAM, data = stat_class)
summary(fit.M1)
fit.M2 <- multinom(GRADE ~ ABSENT + HW + EXAM, data = stat_class)
summary(fit.M2)

fit.M3 <- multinom(GRADE ~ ABSENT + HW + EXAM + SEX, data = stat_class)
summary(fit.M3)
names(fit.M3)

stat_class$predicted = predict(fit.M3)
fit.M3$softmax

head(ml)

table(stat_class$GRADE , stat_class$predicted)
sum(!(stat_class$GRADE == stat_class$predicted))/length(stat_class$GRADE)
n = nrow(stat_class)
summary(stat_class)
str(stat_class)
dim(stat_class)
head(stat_class)


library(ISLR)
head(Carseats)

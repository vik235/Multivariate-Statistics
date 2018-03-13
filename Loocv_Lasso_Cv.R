######
######Vivek Kumar Gupta, LOOCV , bootstrap , n fold CV based proc in Regression- Lasso and Ridge and LSE 
######
######
#install.packages("glmnet")
library("glmnet")

##Functions 
loocvLassoMSE <- function(data , responseinData , bestlam)
{
  #data : dataframe with just the variables used for modelling including the response
  #responseinData : response predictor in string 
  # bestlam : estimate of lambda based on cv 
  #this routine calculates the standard error of the test MSE estimate based on bootstrap. 
  #resource intensive routine. I have used with B = 100
  n = nrow(data)
  p = ncol(data) - 1
  var.error.loocv = rep(NA, n)
  folds = sample(n , n , replace = FALSE ) # LOOCV folds - test folds 
  
  for(i in 1:n)
  {
    model = as.formula(paste(responseinData , " ~ ."))
    train.mat = data[folds != i , ]
    xtrain = model.matrix(model , data.frame(train.mat))[ , -1]
    ytrain = train.mat[, responseinData]
    
    xtest = model.matrix(model, data[folds == i ,  ])
    ytest = data[folds == i  , responseinData ]
    
    fit.lasso.loocv = glmnet(x = xtrain , y = ytrain , lambda = bestlam , family ="gaussian" , alpha = 1)
    fitcoef = coef(fit.lasso.loocv)
    pred =  xtest %*% fitcoef
    var.error.loocv[i] = (pred - ytest)^2
  }
  
  testMSE_est = mean(var.error.loocv)
  return (testMSE_est)
}

loocvMSE <- function(data , responseinData)
{
  #Loocv proc :data : frame having the only data to be used to model. Response is a string of col which holds the response.
  n = nrow(data)
  p = ncol(data) - 1
  var.error.loocv = rep(NA, n)
  folds = sample(n , n , replace = FALSE ) # LOOCV folds - test folds 
  
  for(i in 1:n)
  {
    model = as.formula(paste(responseinData , " ~ ."))
    train.mat = data[folds != i , ]
    test.mat = model.matrix(model , data[folds == i ,  ])
    fit.lse.data.loocv = lm(model , train.mat)
    fitcoef = coef(fit.lse.data.loocv)
    pred = test.mat[, names(fitcoef)]%*%fitcoef
    var.error.loocv[i] = ((pred - data[folds == i , responseinData])^2)
  }
  
  testMSE_est = mean(var.error.loocv)
  return (testMSE_est)
}

#Read the data in , change the dir
happiness <- read.csv('F:/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/happiness.csv' , header = TRUE)

#Rename to friendly names. Note: Dystopia is actually Dystopia residual (opposite things)
colnames(happiness) <- c("Country" , "Region" , "Religion" , "Religious" , "Economy" , "Family" , "Health" ,
                         "Freedom" , "Trust" , "Generosity" , "Dystopia"
)

#Just work with the data needed
happiness = happiness[ , c( "Economy" , "Family" , "Health" , "Freedom" , "Trust" , "Generosity" , "Dystopia"
)]

n = nrow(happiness)
p = 6 

#To reproduce the results
set.seed(1)


#Test for a single iteration 
var.error.loocv = rep(NA, n)
folds = sample(n , n , replace = FALSE ) # LOOCV folds - test folds 

for(i in 1:n)
{
  train.mat = happiness[folds != i , ]
  test.mat = model.matrix(Dystopia ~. , happiness[folds == i ,  ])
  fit.lse.happiness.loocv = lm(Dystopia ~ . , train.mat)
  fitcoef = coef(fit.lse.happiness.loocv)
  pred = test.mat[, names(fitcoef)]%*%fitcoef
  var.error.loocv[i] = ((pred - happiness$Dystopia[folds == i])^2)
}

(testMSE_est = mean(var.error.loocv))
#0.3069102
loocvMSE(happiness , "Dystopia")

## Bootstrap procedure for estimating the density of the statistic testMSE 

B = 100
bootstraptestMSE = rep(NA, B)
for(b in 1:B)
{
  bootHappiness = happiness[sample(n , n , replace = TRUE) , ]
  bootstraptestMSE[b] = loocvMSE(bootHappiness , "Dystopia")
  
}

se_testMSE_est = sd(bootstraptestMSE)
#0.03889592
plot(density(bootstraptestMSE))


#####
##### Via Regularization - Lasso 
#####





#Read the data in , change the dir
happiness <- read.csv('F:/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/happiness.csv' , header = TRUE)

#Rename to friendly names
colnames(happiness) <- c("Country" , "Region" , "Religion" , "Religious" , "Economy" , "Family" , "Health" ,
                         "Freedom" , "Trust" , "Generosity" , "Dystopia"
)

#Just work with the data needed
happiness = happiness[ , c( "Economy" , "Family" , "Health" , "Freedom" , "Trust" , "Generosity" , "Dystopia"
)]

n = nrow(happiness)
p = 6 

#To reproduce the results, set it only when needed such as when you sample or do bootstrap etc. 
set.seed(1)

#grab the model matrix and the response vector to be fed to glmnet 
x = model.matrix(Dystopia ~ . , happiness)[ , -1]
y = happiness$Dystopia

#search space for lambda , try different values 
grid <- c( seq( 1, .0001 , length = 1000))
summary(grid)
set.seed(1)
#fit the model 
fit.lasso.happiness = glmnet(x , y ,  alpha = 1 , family = "gaussian" , lambda = grid)
fit.lasso.happiness = glmnet(x , y ,  alpha = 1 , family = "gaussian" ,standardize = FALSE )
plot(fit.lasso.happiness)


#do Cv and get the CV based estimates of mean of test mse for different values of lambda or better let it be default
#note data is aready standardized
#fit.lasso.happiness.cv = cv.glmnet(x , y ,  alpha = 1 , family = "gaussian" , lambda = grid)
fit.lasso.happiness.cv = cv.glmnet(x , y ,  alpha = 1 , family = "gaussian",
                                   type.measure="mse" , standardize = FALSE)

plot(fit.lasso.happiness.cv)
#find the min i.e. optimal value of lambda for which the cvm is the lowest. 
(bestlam = fit.lasso.happiness.cv$lambda.min)
#0.01719596

#get the coefficient
t(predict(fit.lasso.happiness , s =bestlam , type = "coefficients" ))

##Some junk
#transpose the coef to line up with lambda , you also get intercept here 
dim(t(coef(fit.lasso.happiness)[,]))

#to caclculate the l1 norm 
apply(abs((t(coef(fit.lasso.happiness)[,]))[, -1 ]), 1 , sum)
plot(fit.lasso.happiness.cv$lambda, fit.lasso.happiness.cv$cvsd)
plot(1:length(grid) , apply(abs((t(coef(fit.lasso.happiness)[,]))[, -1 ]), 1 , sum))



B = 100
bootstraptestMSE = rep(NA, B)
for(b in 1:B)
{
  bootHappiness = happiness[sample(n , n , replace = TRUE) , ]
  bootstraptestMSE[b] = loocvLassoMSE(bootHappiness , "Dystopia" , bestlam)
  
}

se_testMSE_est = sd(bootstraptestMSE)
#0.03554531

plot(density(bootstraptestMSE))

x = model.matrix(Dystopia ~ . , happiness)[ , -1]
y = happiness$Dystopia
coef(lm(Dystopia ~. , happiness))
t(coef(glmnet(x  , y  , lambda = bestlam , family ="gaussian" , alpha = 1)))

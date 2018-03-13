happiness <- read.csv('happiness.csv' , header = TRUE)

colnames(happiness) <- c("Country" , "Region" , "Religion" , "Religious" , "Economy" , "Family" , "Health" ,
                         "Freedom" , "Trust" , "Generosity" , "Dystopia"
)

happiness = happiness[ , c( "Economy" , "Family" , "Health" , "Freedom" , "Trust" , "Generosity" , "Dystopia"
)]

n = nrow(happiness)
p = 6 


loocvMSE <- function(data , responseinData)
{
  n = nrow(data)
  p = ncol(data - 1) 
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

loocvMSE(happiness , "Dystopia")

## Bootstrap procedure for estimating the density of the statistic testMSE 

B = 10000
bootstraptestMSE = rep(NA, B)
for(b in 1:B)
{
  bootHappiness = happiness[sample(n , n , replace = TRUE) , ]
  bootstraptestMSE[b] = loocvMSE(happiness , "Dystopia")
  
}

se_testMSE_est = sd(bootstraptestMSE)





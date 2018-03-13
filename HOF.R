####
####Vivek Kumar Gupta. classifers - LDA and QDA and their performance measures.
####
####
####

library("MASS")
hof <- read.csv('hof_data.csv' , header = TRUE)
attach(hof)
head(hof)
##
##Function definition : plotclassifierPerformance - To plot the perf KPIs of a 2 class classifier. Returns a data frame as well
##

plotclassifierPerformance <- function (truth , posterior , threshold , dec)
{
  #truth is a vector of true classes coded as Y or N
  #posterior is a vector of posterior probs.
  #threshold : expected to be a vector in the interval [0,1)
  
  #Setup up vectors to be returned as a data frame
  fit.cv.acc = fit.cv.misclassrate = fit.cv.npv = rep(0, length(threshold))
  fit.cv.ppv = fit.cv.Sensitivity  = rep(0, length(threshold))
  fit.cv.Specificity = fit.cv.balacc = rep(0, length(threshold))
  
  for (i in 2:length(threshold)) {
    class = rep('N', length(truth))
    class[posterior[, 2] >= threshold[i] ] = 'Y'
    tab = table(  class , truth)
    fit.cv.acc[i] = mean(class == hof$HOF)
    fit.cv.misclassrate[i] = 1 - fit.cv.acc[i]
    fit.cv.npv[i] = tab[1,1] / sum(tab[1 , ])
    fit.cv.ppv[i] = tab[2,2] / sum(tab[2 , ])
    fit.cv.Sensitivity[i] = tab[2,2]/sum(tab[ , 2])
    fit.cv.Specificity[i] = tab[1,1]/sum(tab[ , 1])
    fit.cv.balacc[i] = .25* fit.cv.Sensitivity[i] + .75 * fit.cv.Specificity[i]
    
  }
  #This handles when index on kappa is 0 
  class[posterior[, 2] >= threshold[1] ] = 'Y'
  tab = table(  class , hof$HOF)
  fit.cv.acc[1] = mean(class == hof$HOF)
  fit.cv.misclassrate[1] = 1 - fit.cv.acc[1]
  fit.cv.npv[1] = 0
  fit.cv.ppv[1] = fit.cv.acc[1]
  fit.cv.Sensitivity[1] = 1
  fit.cv.Specificity[1] = 0
  fit.cv.balacc[1] = .25* fit.cv.Sensitivity[1] + .75 * fit.cv.Specificity[1]
  par(mfrow = c(2,1))
  
  plot (fit.cv.ppv , type = "n" , xlab = "threshold values",  ylab = "Classification KPI's",
        main = " Plot of Accuracy rates vs threshold's ",
        xlim = c(0,.5) , ylim = c(0  ,1 ) )
  lines( threshold , fit.cv.acc , col = 1 ,  type = 'l' , lwd = 2 , lty = 2)
  lines( threshold, fit.cv.misclassrate,  col = 2 , type = 'l' , lwd = 2 , lty = 2)
  lines( threshold, fit.cv.npv,  col = 3 , type = 'l' , lwd = 2, lty = 3)
  lines( threshold, fit.cv.ppv,  col = 4 , type = 'l', lwd = 2, lty = 4)
  lines( threshold, fit.cv.Sensitivity,  col = 5 , type = 'l' , lwd = 2, lty = 5)
  lines( threshold, fit.cv.Specificity,  col = 6 , type = 'l' , lwd = 2, lty = 6)
  lines( threshold, fit.cv.balacc,  col = 8 , type = 'l' , lwd = 3, lty = 7)
  
  
  legend("bottomright", 
         legend = c("1 - Accuracy Rate" , "2 - MissClassification Rate", 
                    "3 - NPV" , "4 - PPV" , "5 - Sensitivity" , 
                    "6 - Specificity", "7 - Balanced Accuracy"
         ),
         cex = 0.8, 
         text.col = c(1:6,8)
         
  )
  plot(1 - fit.cv.Specificity , fit.cv.Sensitivity , type = 'l' , main = "ROC Curve" , xlab = "1- Specificity" 
       , ylab = "Sensitivity")
  mtext(dec , outer = TRUE)
  par(mfrow = c(1,1))
  df = data.frame ("Accuracy" = fit.cv.acc , "MisClass" = fit.cv.misclassrate , "NPV" = fit.cv.npv , 
                   "PPV" = fit.cv.ppv, "Sensitivity" = fit.cv.Sensitivity , 
                   "Specificity" = fit.cv.Specificity, "BalAccuracy" = fit.cv.balacc)
  return(df)
}



#Kappa : for hyperparameter tuning 
kappa <- seq(from = 0, to = 0.5, by = 0.01)

###
###Fit on all and asses trainign error rate
###

lda.fit <- lda(hof$HOF ~ hof$H + hof$HR + hof$RBI + hof$AVG + hof$SLG + hof$OBP  )
lda.predict = predict(lda.fit)
perfLDA = plotclassifierPerformance(hof$HOF , lda.predict$posterior , kappa , "LDA - No Validation")


###
###CV , LOOCMV
###

lda.fit.cv <- lda(hof$HOF ~ hof$H + hof$HR + hof$RBI + hof$AVG + hof$SLG + hof$OBP , CV = TRUE )
perfLDA.CV = plotclassifierPerformance(hof$HOF , lda.fit.cv$posterior , kappa , "LDA - LOOCMV")


###
###CV , LOOCMV - QDA
###

qda.fit.cv <- qda(hof$HOF ~ hof$H + hof$HR + hof$RBI + hof$AVG + hof$SLG + hof$OBP , CV = TRUE )
names(qda.fit.cv$xlevels)

table (qda.fit.cv$class , hof$HOF)
table (hof$HOF , qda.fit.cv$class )


perfQDA.CV = plotclassifierPerformance(hof$HOF , qda.fit.cv$posterior , kappa , "QDA - LOOCMV")


####LDA vs QDA on Balanced accuracy measure 

plot (perfQDA.CV$BalAccuracy , type = "n" , xlab = "Kappa values",  ylab = "Classification KPI - Balanced Accuracy ",
      main = "Plot of Balanced accuracy rate vs kappa's - LDA and QDA ",
      xlim = c(0,.5) , ylim = c(0  ,1 ) )

lines( kappa , perfQDA.CV$BalAccuracy , col = "green" ,  type = 'l' , lwd = 2 )
lines( kappa , perfLDA.CV$BalAccuracy , col = "blue" ,  type = 'l' , lwd = 2 )

legend("bottomright", 
       legend = c("1 - LDA " , "2 - QDA"
       ),
       cex = 2, 
       text.col = c("blue","green")
       
)

optimal_kappa_qda  = kappa[which(perfQDA.CV$BalAccuracy == max(perfQDA.CV$BalAccuracy ))]
Qda.maxBalAcc = perfQDA.CV$BalAccuracy[which(kappa == optimal_kappa_qda[1] )]
Qda.maxSens = perfQDA.CV$Sensitivity[which(kappa == optimal_kappa_qda[1] )]
Qda.maxSpec = perfQDA.CV$Specificity[which(kappa == optimal_kappa_qda[1] )]
Qda.maxPPV = perfQDA.CV$PPV[which(kappa == optimal_kappa_qda[1] )]
Qda.maxNPV = perfQDA.CV$NPV[which(kappa == optimal_kappa_qda[1] )]
cbind(kappa = optimal_kappa_qda[1] ,Qda.maxBalAcc , Qda.maxSens , Qda.maxSpec , Qda.maxPPV , Qda.maxNPV )

#
# kappa Qda.maxBalAcc Qda.maxSens Qda.maxSpec Qda.maxPPV Qda.maxNPV
#  0.31     0.9486894   0.9591837   0.9451913       0.47  0.9978166
#

optimal_kappa_lda  = kappa[which(perfLDA.CV$BalAccuracy == max(perfLDA.CV$BalAccuracy ))]
Lda.maxBalAcc = perfLDA.CV$BalAccuracy[which(kappa == optimal_kappa_lda[1] )]
Lda.maxSens = perfLDA.CV$Sensitivity[which(kappa == optimal_kappa_lda[1] )]
Lda.maxSpec = perfLDA.CV$Specificity[which(kappa == optimal_kappa_lda[1] )]
Lda.maxPPV = perfLDA.CV$PPV[which(kappa == optimal_kappa_lda[1] )]
Lda.maxNPV = perfLDA.CV$NPV[which(kappa == optimal_kappa_lda[1] )]
cbind(kappa = optimal_kappa_lda[1] ,Lda.maxBalAcc , Lda.maxSens , Lda.maxSpec , Lda.maxPPV , Lda.maxNPV )

#
#      kappa Lda.maxBalAcc Lda.maxSens Lda.maxSpec Lda.maxPPV Lda.maxNPV
#     0.05     0.9201982   0.8979592   0.9276112  0.3859649  0.9944568
#

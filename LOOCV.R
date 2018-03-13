kappa <- seq(from = 0, to = 0.5, by = 0.01)
n = nrow(hof)
m = length(kappa)
class = rep(0 , nrow(hof) )
error = matrix(rep(0 , n*m ) , nrow = n , byrow = TRUE)

for (i in 1:nrow(hof)) {
  train = (1: nrow(hof))[-i]
  lda.fit = lda(HOF ~ H + HR + RBI + AVG + SLG + OBP , data = hof, subset = train  )
  lda.predict = predict(lda.fit , hof[i , ] )    
  for (k in 1:length(kappa)) {
        #max is to handle the situation when k = 0 
          class[i] = colnames(lda.predict$posterior)[max(which(lda.predict$posterior >= kappa[k]))]
          if(class[i] != hof$HOF[i])
          error[i , k] = 1 
      }  
}

table(error)


error[1,]

class
warnings()
str(hof)

error[1, 2] 
newdata = as.data.frame(hof[1 , c("H" , "HR" , "RBI" , "AVG" , "SLG" ,"OBP")])
train = (1: nrow(hof))[-i]
lda.fit = lda(HOF ~ H + HR + RBI + AVG + SLG + OBP , data = hof,  subset = train)
lda.predict = predict(lda.fit , hof[1 , ] )    
head(hof)
index(hof[-i , 3]  )
(1: nrow(hof))[-1]

colnames(lda.predict$posterior)

colnames(lda.predict$posterior)[max(which(lda.predict$posterior >= kappa[1]))]

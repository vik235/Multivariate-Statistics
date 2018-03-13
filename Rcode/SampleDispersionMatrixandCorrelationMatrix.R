diabetes= read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/TextBook/multivariate_analysis - 3rd Ed/T3_5_DIABETES.DAT', header = F, sep = "")
colnames(diabetes) = c("Patient","y1","y2","x1","x2","x3")
diabetes
#Sample mean vector 
Y=as.matrix(diabetes[,2:6])
n=nrow(diabetes)
j=matrix(rep(1,n),nrow = n,ncol = 1,byrow = T)
samplemeanvector=(1/n)*(t(Y)%*%j)


X=calcium[,2:4]
Y=as.matrix(X)
n=nrow(calcium)
I=diag(x=1,n,n)
J=matrix(c(rep(1,n*n)), nrow = n,ncol = n, byrow = T)
S=1/(n-1)*(t(Y)%*%(I - (1/n)*J)%*%Y)

D=diag(sqrt(diag(S)))
R=solve(D)%*%S%*%solve(D)
cor(Y)

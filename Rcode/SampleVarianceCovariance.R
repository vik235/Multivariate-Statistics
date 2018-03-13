calcium= read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/TextBook/multivariate_analysis - 3rd Ed/T3_4_CALCIUM.DAT', header = F, sep = "")
colnames(calcium) = c("Loc","y1","y2","y3")
X=calcium[,2:4]
Y=as.matrix(X)
n=nrow(calcium)
I=diag(x=1,n,n)
J=matrix(c(rep(1,n*n)), nrow = n,ncol = n, byrow = T)
S=1/(n-1)*(t(Y)%*%(I - (1/n)*J)%*%Y)

S


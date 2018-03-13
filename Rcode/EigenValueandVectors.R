A =matrix(c(1,2,2,-2),nrow = 2,ncol = 2,byrow = TRUE)
ev=eigen(A)
ev$vectors

v=sqrt(5)

C= matrix(c(-2/v,1/v,-1/v,-2/v),nrow = 2,ncol = 2,byrow = TRUE)
D=matrix(c(2,0,0,-3),nrow = 2,ncol = 2,byrow = TRUE)
C%*%D%*%t(C)

B=solve(A)
1/6
18*18
18*6

a=324
b=-54
c=-20
(b^2) -4*a*c
t=sqrt(((b^2) -4*a*c))
(-b+t)/(2*a)
(-b-t)/(2*a)

eigen(B)

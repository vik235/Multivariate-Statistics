##x is the sample observation vector, mvtnorm density to be evaluated for
##mu population mean vector
##sigma population dispersion matrix 
## usage pdf_x(x , mu , sigma)
## outs density 
pdf_x <- function(x,y)
{
  z=c(x,y)
  mu = c(1 , -1)
  sigma = matrix(c(1 ,  -1.6 ,  -1.6 , 4) , nrow = 2 , ncol = 2, byrow = TRUE)
  p = length(mu) 
  sigma_inverse = solve(sigma)
  det_sigma = det(sigma)
  const= (2*pi)^(p/2)*(det_sigma)^(1/2)
  density = const^(-1) * exp(-(t(z - mu)%*%sigma_inverse%*%(z - mu))*(1/2))
  return(density)
}


##Testing function 
x = c(3.1,-1.1 )
y=c(-2.1,1.1)
pdf_x(1.1,-1.1)

#Creating the sequences around the mean of each variable. 

x=seq(-2,4,length=50)
y=seq(-13,11,length=50)

#creating the 50x50 matrix of densities. 
z=outer(x , y , Vectorize(pdf_x))


#Drawing the surface. 
persp(x,y,z, theta=40, phi=30,ticktype = "detailed", border=2 ,
      main= "Bivariate Normal Density Plot" ,
      xlab="x1", ylab="y1", zlab="Density")
dev.off()

####
####Vivek Kumar Gupta, Assignment 3 Problem 5. MultiDimensional Scaling 
####
####

##Read the data in 
X <- as.matrix(read.table("T12-8(1).DAT", header = FALSE))

#Set row and column identifiers
pot_type <- c("A", "B", "C", "D")
pot_site <- paste("P", 0:6, sep = "_")
rownames(X) <- pot_site
colnames(X) <- pot_type

#Compute the diss matrix 
D = dist((X) , method = "euclidean")

#Employ MDS, with ordination p = 2, note cmdscale does not employ stress per se but attemtps to maximize variances along the 
#axes using eigne analysis just like PCA

#Also set params for returning eigen representation
mds = cmdscale(D , k = 2 , eig = T)

#Plot the data , SCREE Plot
plot(mds$eig, type = "b" , ylab = "Eigen values" , xlab = "No of dimensions" , main = "Scree plot of MDS")
#It appears that we can reasonably represent this in 2 Dimension

plot(mds$points[,1] , mds$points[,2] , type = "p" , pch = "+" , col =2  , xlab = "Dimension 1" , ylab = "Dimension 2" , main = "MDS Pot of the pottery data")
text(mds$points[,1] , mds$points[,2], labels(D), cex=.9 ,  col=4)

####Comments 
#We can see that P_6 site is really different and distinct than the rest of the sites. 
#P_0 and P_3 are relatively close to each other
#P_4, P_5, P_1 and P_2 are reasonably far apart. 

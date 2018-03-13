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
dev.off()
par(mfrow=c(1,2))
plot(mds$points[,1] , mds$points[,2] , type = "p" , pch = "+" , col =2  , xlab = "Dimension 1" , ylab = "Dimension 2" , main = "MDS Pot of the pottery data")
text(mds$points[,1] , mds$points[,2], labels(D), cex=.9 ,  col=4)

####Comments 
#We can see that P_6 site is really different and distinct than the rest of the sites. 
#P_0 and P_3 are relatively close to each other
#P_4, P_5, P_1 and P_2 are reasonably far apart. 


#####
##### BiPlot Analysis . 5b
#####
#####

pca = princomp(X) 
biplot( pca , col = c(2,3) , main = "Biplot of Pottery Data" , xlab = "Princ Comp 1" , ylab = "Princ Comp 2")

##
##Interpretation
##

#Biplot of the pottery data conveys alot of information as compared to MDS/PCA plots as we can see above that it gives a
#consolidated picture of principal components and the contribution in magnitude of the variables involved in the dataset.
#Each plotted points represent the obervation vector in the data matrix. Also provided is a representation of the potsherds
#(A,B,C,D) by the vectors(shown in green). The length of the vector as shown in the picture does not convey much 
#Thus, we can see that sites P_0 and P_3 are closer to each other and this similarity is due to the prevalance 
#of potsherd D and A at both these sites. P_2 and P_1 due to large values of A. P_6 represents large values of C
#As compared to MDS plot, biplot does convey a lot of message giving information not only about the principal components 
#but also of the variables involved and their individual contribution. 
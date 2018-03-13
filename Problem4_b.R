####
####Vivek Kumar Gupta, STAT 636 HW 3 , Problem 4 Solution 
####

##
##Part a analysis 
##

#Read the data in 
cereals = read.csv('cereal.csv', header = T)
#Compute the dissimilarity matrix
D = dist(cereals[,c(-1)])

#Run Hierarchical clustering algorithm on the unscaled data. Use complete linkage method to commute similarity.
hclus = hclust(D , method = "complete" )

#, labels = as.factor(cereals[,1])
#Plot the dendogram, with a labelled data
plot(hclus ,  xlab = "Cereal brands" ,
          ylab = "Height" , main = "Cereals Cluster Dendogram - Complete Linkage")
#It looks like we have atleast 3 distinctinve clusters discovered via dendogram

#Plot the heights of various merges, to figure out how many clusters are formed.Note: Heights returned 
#are sorted already
plot(hclus$height, type = "b", ylab = "Heights of merges" , xlab = "Indices" , main = "Plot of merge heights" , col = 2)

#cluster 1
cereals[c(15, 19, 24, 21, 36, 26, 43, 41, 42), ] # Get this via order value and map it to labels returned via hclust
c1 = c(15, 19, 24, 21, 36, 26, 43, 41, 42)
sort(c1)
#15 19 21 24 26 36 41 42 43

#cluster 2 
cereals[c(2,6,33,7,5,20,35,17,1,4,28,3,8,30,12,25,37,23,38,39,10,32,40,31,9,14,16),]
c2 = c(2,6,33,7,5,20,35,17,1,4,28,3,8,30,12,25,37,23,38,39,10,32,40,31,9,14,16)
sort(c2)
#1  2  3  4  5  6  7  8  9 10 12 14 16 17 20 23 25 28 30 31 32 33 35 37 38 39 40 
sort(c(2,6,33,7,5,20,35,17,1,4,28,3,8,30,12,25,37,23,38,39,10,32,40,31,9,14,16))

#cluster 3 
cereals[c(18,29,11,22,27,13,34),]
c3 = c(18,29,11,22,27,13,34)
sort(c3)
#11 13 18 22 27 29 34

hclus$height
#Note that cluster 2 and 3, toegther are contained in a
#merged cluster which is separate from cluster 1 


###
###Part b analysis 
###
###

#Run the Kmeans algorithm with 3 clusters 
kmean = kmeans(cereals[,-1] , centers = 3)

#Do a PCA on cereals
pca = princomp(cereals[,-1] )

#Compute the Principal Components
pc = as.matrix(cereals[,-1])%*%pca$loadings

#Plot the PCs, first 2 and color code them along with the cluster membership as retruned by Kmeans
plot(pc[,1], pc[,2] , col = kmean$cluster+1 , type = "n" , main = "Clusters in cereals", xlab = "Principal Component 1" ,ylab = "Principal Component 1")
text(pc[,1], pc[,2],labels(row.names(cereals)) ,cex=.8, col = kmean$cluster+1)

#Observations belonging to first cluster 
rownames(cereals[which(kmean$cluster == 1),])

#Observations belonging to 2nd cluster 
rownames(cereals[which(kmean$cluster == 2),])

#Observations belonging to 3rd cluster 
rownames(cereals[which(kmean$cluster == 3),])

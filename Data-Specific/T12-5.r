####
#### Data collected on 22 U.S. public utility companies for the year 1975.
####

## Load data.
dta <- read.table("T12-5.DAT", header = FALSE)
company <- as.character(dta[, 9])
X <- as.matrix(dta[, -9])
rownames(X) <- company
colnames(X) <- paste("X", 1:8, sep = "_")

## Output to csv.
write.csv(X, "../utilities.csv", row.names = TRUE)

##
## Clustering variables.
##

## Correlations as distance.
dd <- as.dist((1 - cor(X)) / 2)

## Hierarchical clustering with complete linkage.
hc <- hclust(dd, method = "complete")

##
## Clustering companies.
##

## Standardize the variables.
X_st <- scale(X, center = TRUE, scale = TRUE)

## Euclidean distance.
dd <- dist(X_st)

hc <- hclust(dd, method = "average")
plot(hc)

##
## K-means.
##

km <- kmeans(X, 5, algorithm = "MacQueen")

## An attempt to visualize the results. We could make a color-coded plot of the first 
## two PCs. There really doesn't appear to be much structure in these data.
pca <- prcomp(X, center = TRUE, scale = TRUE)
Y <- scale(X, center = TRUE, scale = TRUE) %*% pca$rotation[, 1:2]

plot(Y, xlab = expression(PC[1]), ylab = expression(PC[2]), pch = 20, col = km$cluster)

## Here is another function to consider, although I can't tell what it's doing exactly.
library(useful)
plot.kmeans(km, data = X)

##
## MDS.
##

mds_fit <- cmdscale(dd, k = 2); mds_fit[, 1] <- -mds_fit[, 1]

plot(mds_fit[, 1], mds_fit[, 2], xlab = "", ylab = "", type = "n")
text(mds_fit[, 1], mds_fit[, 2], labels = rownames(X), cex = 0.7)

write.csv(mds_fit, "figures/utilities_mds.csv")

##
## Biplot.
##

pdf("figures/utilities_biplot.pdf")
biplot(prcomp(X_st))
dev.off()












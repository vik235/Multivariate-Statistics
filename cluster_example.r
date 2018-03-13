####
#### Toy example to illustrate hierarchical clustering.
####

D <- as.dist(matrix(c(0, 1, 11, 5, 1, 0, 2, 3, 11, 2, 0, 4, 5, 3, 4, 0), nrow = 4))

## Hierarchical clustering: Step 1 = join individuals 1 and 2 into {1, 2}. Now have the 
## "clusters" {1, 2}, {3}, {4}.
hc_single <- hclust(D, method = "single")
hc_single
plot(hc_single)

hc_complete <- hclust(D, method = "complete")
plot(hc_complete)

hc_average <- hclust(D, method = "average")
plot(hc_average)

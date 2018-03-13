##
## Clustering some simulated data.
##

set.seed(1)

## Fake data with 2 clusters. Clear separation between the two clusters.
Y_2 = rbind(matrix(rnorm(25 * 2, 0, 0.5), nrow = 25), matrix(rnorm(25 * 2, 3, 0.5), 
                                                             nrow = 25))
KM_2 = kmeans(Y_2, 2)
HC_2 = hclust(dist(Y_2), "average")
?dist
par(mfrow = c(1, 2))
plot(Y_2[, 1], Y_2[, 2], col = KM_2$cluster)
points(KM_2$centers[, 1], KM_2$centers[, 2], col = 1:2, pch = 20, cex = 5)
points(Y_2[, 1], Y_2[, 2], col = c(rep("black", 25), rep("red", 25)), pch = 20, cex = 0.6)
plot(HC_2)

## Less separation. Now some overlap between the clusters.
Y_2 = rbind(matrix(rnorm(25 * 2, 0, 0.5), nrow = 25), 
            matrix(rnorm(25 * 2, 1, 0.5), nrow = 25))
KM_2 = kmeans(Y_2, 2)
HC_2 = hclust(dist(Y_2), "average")

par(mfrow = c(1, 2))
plot(Y_2[, 1], Y_2[, 2], col = KM_2$cluster)
points(KM_2$centers[, 1], KM_2$centers[, 2], col = 1:2, pch = 20, cex = 5)
points(Y_2[, 1], Y_2[, 2], col = c(rep("red", 25), rep("black", 25)), pch = 20, cex = 0.6)
plot(HC_2)

## Fake data with 5 clusters, little separation. Much harder to see clustering now. One 
## would be more likely to conclude that there are two clusters than five here (based on 
## the plot of merge heights and by visual inspection of the dendogram). If we choose two 
## clusters, it looks like most of the observations with means +/- 1 group together, and 
## most of the observations with mean +/- 0.5 group together.
Y_5 = rbind(matrix(rnorm(10 * 2, 0, 0.5), nrow = 10), 
            matrix(rnorm(10 * 2, 1, 0.5), nrow = 10), 
            matrix(rnorm(10 * 2, -1, 0.5), nrow = 10), 
            matrix(rnorm(10 * 2, -0.5, 0.5), nrow = 10), 
            matrix(rnorm(10 * 2, 0.5, 0.5), nrow = 10))
KM_5 = kmeans(Y_5, 10)
HC_5 = hclust(dist(Y_5), "average")
KM_5
par(mfrow = c(1, 2))
plot(Y_5[, 1], Y_5[, 2], col = KM_5$cluster)
points(KM_5$centers[, 1], KM_5$centers[, 2], col = 1:5, pch = 20, cex = 5)
plot(HC_5)

par(mfrow = c(1, 1))
plot(HC_5$height)

## Fake data with no clusters. A clustering algorithm will always return an "optimal" 
## assignment of observation to groups. That doesn't mean that the grouping is real. Here
## is an example where there is *no* grouping. There is no obvious place in the dendogram 
## where group separation jumps, and the plot of merge heights has no obvious "scree."
set.seed(101)
Y_0 = matrix(rnorm(50 * 2, 0, 0.5), nrow = 50)
KM_0 = kmeans(Y_0, 3)
HC_0 = hclust(dist(Y_0), "average")

par(mfrow = c(1, 2))
plot(Y_0[, 1], Y_0[, 2], col = KM_0$cluster)
points(KM_0$centers[, 1], KM_0$centers[, 2], col = 1:3, pch = 20, cex = 5)
plot(HC_0)

par(mfrow = c(1, 1))
plot(HC_0$height)


diag(solve(KM_5$tot.withinss)%*%KM_5$betweenss)

mclust()

install.packages("mclust")

library(mclust)

m = (mclustBIC(Y))

mod1 = Mclust(Y_5)
plot(mod1)
0


1953.54 + 578.26



store = c('A' , 'B' , 'C' ,'D')
psi = c( 1/16 , 2/16 , 3/16, 10/16)
t_i = rep(75,4)
t_psi_est = t_i / psi
#Expected value of t_psi
t_psi_avg = sum(psi * t_psi_est)
#300
var_t_psi = sum(psi * (t_psi_est - t_psi_avg)^2 )
#154488


((450000+70000+137000+30000+15000 + 60000 + 400000)*64)+ 15000000 + 8500000

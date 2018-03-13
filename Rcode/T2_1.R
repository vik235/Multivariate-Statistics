
consumer  = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/consumer_pref.csv', header = T)
attach(consumer)

n = 100 
p = 5 
nu = n - 1 
S = cov(consumer)
x_bar = apply(consumer,2 , mean)

qqnorm(Flavor)
qqline(Flavor)



c = sqrt((nu*p/(nu - p + 1))*qf(1 - .05 , p , (nu - p + 1)))

eigen = eigen(S)

sqrt((eigen$values)/n) * (c)

eigen$vectors


#Bonfer
m = 5
a = c(1 , 0 , 0 , 0 , 0 )
se = qt(1 - .05/(2*m), n - 1)*sqrt(diag(S)/n)

#Sweat : 
t(a)%*%x_bar + as.vector(se)

x_bar[1] + c(-1,1)* se[1]
x_bar[1] + se[1]

mu_0 = rep(0, p)

#T2 test 
stat = n* t(x_bar - mu_0)%*%solve(S)%*%(x_bar - mu_0)

pval = 1 - pf(stat * (nu - p + 1)/(nu*p) , p , (nu - p + 1))

TCrit = ((nu - p + 1)/(nu*p))^(-1) * qf(1 - .05 , p ,  (nu - p + 1))

HotellingsT2Test(consumer , test = "f")

mu_0 = rep(0, p)
consumer_o = consumer - matrix(rep(x_bar , n), nrow = n , byrow = T) + matrix(rep(mu_0 , n), nrow = n , byrow = T) 
T2 = n* t(x_bar - mu_0)%*%solve(S)%*%(x_bar - mu_0)
B = 1000
T2_0 = rep(NA,B)

for (i  in 1:B) {
x_b = consumer_o[sample(1:n, replace = T),]
x_bar_b = apply(x_b, 2 , mean)
S_b = cov(x_b)
T2_0[i] = n* t(x_bar_b - mu_0)%*%solve(S_b)%*%(x_bar_b - mu_0)
}

which(T2_0 >= T2)
mean(T2_0 >= T2)

B <- 1000
T2_b <- numeric(B)
set.seed(101)
X_0 <- scale(consumer, center = TRUE, scale = FALSE)
for(b in 1:B) {
  4
  X_b <- X_0[sam(1:n, replace = TRUE), ]
  x_bar_b <- colMeans(X_b)
  S_b <- var(X_b)
  T2_b[b] <- n * t(x_bar_b - mu_0) %*% solve(S_b) %*% (x_bar_b - mu_0)
}
p_val_boot <- sum(T2_b >= T2) / B

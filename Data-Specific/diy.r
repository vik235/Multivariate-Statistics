####
#### Do-it-yourself data.
####

dta <- read.csv("diy.csv")

## Multiple correspondence analysis.
library(ca)

mjca_out <- mjca(dta)
mjca_out
plot(mjca_out)

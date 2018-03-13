####
#### Data on 600 students, with three psychological variables ('control', 'concept', 
#### and 'motivation') and four academic variables ('read', 'write', 'math', and 
#### 'science').  
####

library(CCA)

dta <- read.csv("http://www.ats.ucla.edu/stat/data/mmreg.csv")
colnames(dta) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
    "Science", "Sex")
summary(dta)

dta_psych <- dta[, 1:3]
dta_acad <- dta[, 4:7]

cc1 <- cc(dta_psych, dta_acad)

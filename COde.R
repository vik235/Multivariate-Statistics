##### MV Analysis
##### Vivek Kumar Gupta, 9/5/2017

## Read in the dataset
oxygen <- read.delim('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/636/Data/oxygen.DAT'
                     , header = FALSE
                    , sep = "")

## Set column names of the dataframe as the header is not there in the input dataset
colnames(oxygen) <- c("X_1", "X_2", "X_3", "X_4", "Gender")

## Set n's for each category of Gender to be used in calculating sample std deviation 
nrow_female = nrow(oxygen[oxygen$Gender == 'female', 1:4])
nrow_male = nrow(oxygen[oxygen$Gender == 'male', 1:4])

mean_male = apply(oxygen[oxygen$Gender == 'male' , 1:4] , 2 , mean)
sd_male = (apply(oxygen[oxygen$Gender == 'male', 1:4] , 2 , sd)) * (nrow_male - 1)/nrow_male
mean_female = apply(oxygen[oxygen$Gender == 'female' , 1:4] , 2 , mean)
sd_female = (apply(oxygen[oxygen$Gender == 'female', 1:4] , 2 , sd)) * (nrow_female - 1)/nrow_female

## Setting up for producing the desired output.
variable<- c("X_1", "X_2", "X_3", "X_4")

data.frame("var" = variable , "Male,ybar" = mean_male ,
              "Male,sd" =  sd_male , "Female,ybar" = mean_female ,
              "Female,sd" =  sd_female   )

#Boxplots of the data to understand distribution (sample) from plots.
dev.off()
par(mfrow = c(2,2))
boxplot(oxygen$X_1 ~ Gender)
mtext("Variable X_1")
boxplot(oxygen$X_2 ~ Gender)
mtext("Variable X_2")
boxplot(oxygen$X_3 ~ Gender)
mtext("Variable X_3")
boxplot(oxygen$X_4 ~ Gender)
mtext("Variable X_4")

pairs(oxygen)
?identify(Gender$X_1)

plot(Gender$X_1, Gender$)

coplot(X_1 ~ X_3 | Gender , oxygen)

###
#Que. 1
###

##(i)

setwd("D:/School/3/Applied Analytics")

#Importing the data
x <- read.csv("D:/School/3/Applied Analytics/DATASET.csv", stringsAsFactors = TRUE)
str(x)

#The dependent variable is approved
#Changing the dependent variable labels
x$Approved <- factor(x$Approved, levels = c("0","1"), labels = c("Approved","Not approved"))
table(x$Approved)

#Exploring the independent variables
summary(x$Age)
summary(x$YearsEmployed)
#Both variables are positively skewed

#Randomize the data order
set.seed(12345)
x_rand <- x[order(runif(690)), ]

#Checking to see if the data was randomized correctly
head(x$Debt)
head(x_rand$Debt)
#The data has been randomized successfully

#Splitting into training and testing data sets (80/20 split)
x_train <- x_rand[1:552, ]
x_test <- x_rand[553:690, ]

prop.table(table(x_train$Approved))
prop.table(table(x_test$Approved))
#It is a fairly equal split

#Implementing a c5.0 model
library(C50)
x_model <- C5.0(x_train[-16],x_train$Approved)
x_model
summary(x_model)
#The model incorrectly predicts 65 outputs (11.8%)

#Predicting the model 
x_pred <- predict(x_model, x_test)
library(gmodels)
CrossTable(x_test$Approved,x_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#The model incorrectly predicts 18 outcomes (13.04%)
#TP=70, FP=12, TN=50, FN=6


#(iii)

#Implementing a OneR model
library(RWeka)
x_1R <- OneR(Approved ~ ., data = x)
x_1R
summary(x_1R)
#From the variable prior default, applicants with a value <0.5 were approved and those with >= 0.5 were not approved
#The model correctly estimates 590 instances and 100 incorrect instances (error rate of 14.5%)


#(iv)

#Improving model performance
#For c5.0 algorithm we can implement adaptive boosting with 10 trials
x_boost10 <- C5.0(x_train[-16], x_train$Approved, trials = 10)
x_boost10
summary(x_boost10)
#The model errors reduce to 45 (8.15%)

#Predicting the new model
x_boost_pred10 <- predict(x_boost10, x_test)
CrossTable(x_test$Approved,x_boost_pred10, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#The model incorrectly predicts 16 outcomes (11.6%)
#TP=69, FP=9, TN=53, FN=7

#For the one ripper model we can implement a JRip algorithm
x_JRip <- JRip(Approved ~ ., data = x)
x_JRip
#From JRip we get two rules; If approved >=1 approved otherwise it is not approved



###
#Que. 2
###

##(i)

#Importing the data set
y <- read.csv("D:/School/3/Applied Analytics/DATA.csv")
str(y)

#Exploring the correlation between the variables
pairs(y[c("mpg","tax","mileage","engineSize")])
library(psych)
pairs.panels(y[c("mpg","tax","mileage","engineSize")])
#Miles per gallon has a moderately negative correlation with tax and a weak neagtive correlation to engine size
#Mileage has a weak negative correlation to engine size and also miles per gallon
#All the four variables are skewed to the left

#(ii)
#Training a linear regression model
y_model <- lm(price ~ model + year + price + transmission + mileage 
              + fuelType + tax + mpg + engineSize, data = y)
summary(y_model)
#The model correctly predicts 89.14% (from the adjusted R-squared)
#All variables apart from semi-auto transmission and model s5 are statistically significant
#All variables have a positive effect by their respective estimates to price apart from tax, miles per gallon, petrol fuel, manual transmission and mileage


#(iii)

#Adding binary terms
summary(y$mpg)
##mpg has a mean of 50.77
summary(y$mileage)
##mileage has a mean of 24827

#Converting the variables to binary terms
y$mpg2 <- ifelse(y$mpg >= 50.77,1,0)
y$mileage2 <- ifelse(y$mileage >= 24827,1,0)

#Adding interaction terms to become:
#mpg*engineSize and year*mileage

#Incorporating the changes to the model 
y_model2 <- lm(price ~ model + year + price + transmission + mileage + mileage2
               + fuelType + tax + mpg + mpg2 + engineSize + mpg*engineSize
               + year*mileage , data = y)
summary(y_model2)
#The model's accuracy has increased to 90.5%
#There is only one statistically insignificant variable i.e. model s5
#Only five variavles affect price negatively with respect to their estimates i.e. petrol fuel, tax, mpg*engineSize, year*mileage and manual transmission
insurance <- read.csv("D:/School/3/Applied Analytics/insurance.csv")
str(insurance)

#Exploring the dependent var (charges)
summary(insurance$charges)
##Mean > Median so the data is right-skewed
hist(insurance$charges) #To confirm

#Check distibution of the variables
table(insurance$region)
table(insurance$sex)
table(insurance$smoker)
table(insurance$children)

#correlation matrix
cor(insurance[c("age","bmi","children","charges")])

#Scatterplot matrix (collection of scatterplots in matrix format)
pairs(insurance[c("age","bmi","children","charges")])
##Enhanced scatterplot matrix
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])
##Diagonals depict the histogram of the variables

#Linear regression model
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model1 <- lm(charges ~ . , data=insurance)
ins_model

#Evaluate model performance
summary(ins_model)
##The model correctly predicts 74.94% (R-squared)

#Improving model performance
##Adding non-linear relationships (for age)
insurance$age2 <- insurance$age^2
##Converting a numeric var to a binary var (bmi)
insurance$bmi30 <- ifelse(insurance$bmi >= 30 ,1 ,0)
##Adding interaction effects (two features have a combined effect)
###E.g. bmi30:smoker or bmi30*smoker

#Apply improvements to the model
ins_model2 <- lm(charges ~ age + age2 + children + bmi + bmi30 + sex + smoker + bmi30*smoker + region, data = insurance)
summary(ins_model2)
##The model's accuracy has improved to 86.53%


#Using caret model tp evaluate
j <- train(charges ~., data = insurance, method = "lm")
j

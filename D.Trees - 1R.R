setwd("D:/School/3/Applied Analytics")
mushroom <- read.csv("D:/School/3/Applied Analytics/mushrooms.csv", stringsAsFactors = TRUE)

#Explore data
str(mushroom)
##Veil type has been coded incorrectly so we drop it
mushroom$veil.type <- NULL

#Dependent variable
table(mushroom$type)
##Rename the variable type
mushroom$type <- factor(mushroom$type, levels = c("e","p"), labels = c("edible","poisonus"))
summary(mushroom$type)

#Training the model - 1R implementation
library(RWeka)
mushroom_1R <- OneR(type ~ ., data = mushroom)
mushroom_1R

#Evaluate model performance
summary(mushroom_1R)

#Improving model performance
mushroom_JRip <- JRip(type ~ ., data = mushroom)
mushroom_JRip
##There are 9 rules (same as ifelse statements)
setwd('C:\\Users\\esaia\\Desktop\\Year3\\SEM2\\Applied Analytics\\Analytics For finance')

# importing the Credit data
data<-read.csv('Credit_Data.csv', stringsAsFactors = TRUE)
str(data)

# extracting the numerical and factor values apart from the 'default' variable
num<-data[,c(3,6,9,12,14,17,19)]
fac<-data[,c(-3,-6,-9,-12,-14,-17,-19)]

# checking the structure of the numeric values
summary(data[c('age','people_under_maintenance')])

# the ranges vary and therefore we normalize using the normalize() fcn
# creating the noramlize fxn
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# normalizing the entire numeric data
num1<-as.data.frame(lapply(num, normalize))
str(num1)


# the factor variables have to be converted to dummy variables 
# for the knn() fxn to work
library(fastDummies)
fac1<-as.data.frame(lapply(fac[2:14], dummy_cols))
str(fac1)

# the new dummy data 'fac1' contains the fators as well
# a new data fac2 is created to have only the dummy data
fac2<-fac1[,c(-1,-6,-12,-23,-29,-35,-40,-44,-49,-53,-57,-62,-65)]
str(fac2)

# the final data with combined variables of num1 and fac2
data1<-cbind(fac2,num1)

# creating the test:300(30%) and train:700(70%) dataset
data_train<-data1[1:700,]
data_test<-data1[701:1000,]

# creating the class labels
facc1<-factor(fac[,1], levels = c('0','1'), labels = c('Default','Not Default'))
data_train_labels<-facc1[1:700]
data_test_labels<-facc1[701:1000]


# using the knn()
library(class)
data_test_pred<-knn(train = data_train, test = data_test, cl = data_train_labels, k=30)

# computing the accuracy of our model
library(gmodels)
CrossTable(data_test_labels, data_test_pred)
# 216 predicted the actual values giving a 72% accuracy
# suppose we change k=31
data_test_pred<-knn(train = data_train, test = data_test, cl = data_train_labels, k=32)
CrossTable(data_test_labels, data_test_pred)
# still the accuracy level lies with the 72% accuracy
# the focus therefore shifts to reduce false positive
# k=32
data_test_pred<-knn(train = data_train, test = data_test, cl = data_train_labels, k=25)

# USING THE Z-SCORE STANDARDIZATION

# standardizing the num data
num_z<-as.data.frame(scale(num))
summary(num_z)

# combining the data
data_z<-cbind(fac2,num_z)


# creating the test:300(30%) and train:700(70%) dataset
data_train_z<-data_z[1:700,]
data_test_z<-data_z[701:1000,]

# creating the class labels
facc1<-factor(fac[,1], levels = c('0','1'), labels = c('Default','Not Default'))
data_train_labels<-facc1[1:700]
data_test_labels<-facc1[701:1000]

# Uisng the knn()
data_test_pred_z<-knn(train = data_train_z, test = data_test_z, cl = data_train_labels, k=30)
CrossTable(data_test_labels, data_test_pred_z)

data_test_pred_z<-knn(train = data_train_z, test = data_test_z, cl = data_train_labels, k=25)
CrossTable(data_test_labels, data_test_pred_z)

data_test_pred_z<-knn(train = data_train_z, test = data_test_z, cl = data_train_labels, k=32)
CrossTable(data_test_labels, data_test_pred_z)

data_test_pred_z<-knn(train = data_train_z, test = data_test_z, cl = data_train_labels, k=30)
CrossTable(data_test_labels, data_test_pred_z)

data_test_pred_z<-knn(train = data_train_z, test = data_test_z, cl = data_train_labels, k=33)
CrossTable(data_test_labels, data_test_pred_z)


#Reading/importing the csv file
wbcd <- read.csv("D:/School/3/Applied Analytics/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)

#Removing the variable I.d.
wbcd<-wbcd[-1]

#Checking the dependent variable
table(wbcd$diagnosis)

#recording the diagnosis variable
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"),labels = c("Benign", "Malignant"))
#prop.table() output
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Summary of the data
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Create a function to normalize the output of the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
##Testing the function
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

#Apply the normalize function to the data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)


#Splitting the data to two (predicting and testing accuracy)
wbcd_train <- wbcd[1:469, ]
wbcd_test <- wbcd[470:569, ]
##Creating a label for the dependent variable
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#Run a k-NN algorithm
library(class)
knn(train= wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=3)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)

#Comparing the test results to our predicted model
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)
##The model predicts 5 false malignant cases

###
#Improving on model performance
##Using z-scores (scale fn. in R)
wbcd_z <- as.data.frame(scale(wbcd[-1]))

##Checking the standardized data
summary(wbcd_z$area_mean)

##Running a k-NN model
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
#The model predicts 5 incorrect malignant cases
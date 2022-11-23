#Loading the data and setting the file directory
setwd("D:/School/3/Applied Analytics")
credit <- read.csv("D:/School/3/Applied Analytics/Credit_Data.csv", stringsAsFactors = TRUE)
str(credit)

#Changing dependent var labels
credit$default <- factor(credit$default, levels = c("0","1"), labels = c("No","Yes"))
table(credit$default)

#Data Cleaning
levels(credit$savings)<-make.names(levels(credit$savings))
levels(credit$account_check_status)<-make.names(levels(credit$account_check_status))
levels(credit$present_emp_since)<-make.names(levels(credit$present_emp_since))
levels(credit$personal_status_sex)<-make.names(levels(credit$personal_status_sex))
levels(credit$property)<-make.names(levels(credit$property))
levels(credit$credit_history)<-make.names(levels(credit$credit_history))
levels(credit$foreign_worker)<-make.names(levels(credit$foreign_worker))
levels(credit$housing)<-make.names(levels(credit$housing))
levels(credit$job)<-make.names(levels(credit$job))
levels(credit$other_debtors)<-make.names(levels(credit$other_debtors))
levels(credit$other_installment_plans)<-make.names(levels(credit$other_installment_plans))
levels(credit$purpose)<-make.names(levels(credit$purpose))
levels(credit$telephone)<-make.names(levels(credit$telephone))

#Creating a simple tuned model
library(caret)
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")
m
#Model chosen; rules = model, winnow = false, accuracy = 74%, Kappa = 35%

#Confusion matrix
p <- predict(m, credit)
table(p,credit$default)
#The model mis-classifies 5 outputs (0.5% error rate) TP=699, FP=1, TN=296, FN=4

#Obtain estimated class
head(predict(m,credit))
head(p)

#Obtain estimated probabilities of each class
head(predict(m, credit, type = "prob"))



##Customizing the tuning process

#To set the re-sampling method of choice (trainControl)
#To specify fn. that will choose the optimal model(selectionFunction)
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")
ctrl

#Creating a grid of parameters to optimize
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")
grid

#Running the cutomized train model
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0", 
            metric = "Kappa", trControl = ctrl, tuneGrid = grid)
m
#The optimal model selected is; trials = 15, model = tree and winnow = FALSE
#Accuracy = 75.9% and Kappa = 38.3%



##Improving model performance (meta-learning)


#Bagging
library(ipred)
set.seed(300)
bag <- bagging(default ~ ., data = credit, nbagg = 25)

credit_pred <- predict(bag,credit)
table(credit_pred, credit$default)
#The model fits the training data better; TP=699, FP=3, TN=297, FN=1

#Checking into future performance
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
m <- train(default ~ ., data = credit, method = "treebag", trControl = ctrl)
m
#Accuracy = 73.1%, Kappa = 31.98%
#Comparing the Kappa statistic the bagged model is at least as good as the customized model

str(svmBag) #Provides a framework for bagging
svmBag$fit
svmBag$pred
svmBag$aggregate
#Applying the three functions
bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred, 
                      aggregate = svmBag$aggregate)

#The new model becomes
set.seed(300)
svmbag <- train(default ~ ., data = credit, method = "bag", 
                control = ctrl, bagControl = bagctrl)
#


#Boosting
library(adabag)
set.seed(300)

#Creating adaboost classifier and prediction fn.
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)

#Instead of predictions the function returns an object with info from the model
head(m_adaboost$class)

#Confusion matrix of the training data is given by
p_adaboost$confusion
#The model makes no errors (over-fitted model)

#Evaluating using 10-cross validation model
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)

#Confusion matrix for 10-cv model
adaboost_cv$confusion

#Evaluate kappa statistic using vcd package
library(vcd)
Kappa(adaboost_cv$confusion)
#kappa = 36.44% 


#Random forests
library(randomForest)

#Testing the model parameters on credit data
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf
#TP=643, FP=57, TN=121, FN=179 with error rate of 23.6%

#Evaluate rf model performance
library(caret)

#setting training control options; 10-cv model repeated 10 times
ctrl <- trainControl(method = "repeatedcv", number = 10,
                     repeats = 10)

#setting up tuning grid
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

#Supplying to the train function
set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf", metric = "Kappa",
              trControl = ctrl, tuneGrid = grid_rf)

#Comparing to a boosted tree
grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")

#Create boosted tree model
set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0", metric = "Kappa",
               trControl = ctrl, tuneGrid = grid_c50)

#Comparing statistics of the two models
m_rf
#The optimal model mtry=16, with a kappa of 36.4%
m_c50
#The optimal model has 40 trials with kappa of 34.53%
##Conclusively, the random forest model performs better than the boosted c5.0

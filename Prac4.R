# load dataset Boston
library("MASS")

myData <- Boston

#split train/test datasets

set.seed(0)
trainIdx <- sample(nrow(myData),round(nrow(myData)*0.6)) # 60% for training
train <- myData[trainIdx,]
test <- myData[-trainIdx,]

#install glmnet package
install.packages("glmnet",dependencies=TRUE)
library(glmnet)
?glmnet

# check data type
str(train)

# fit ridge regression on training data
# medv ~ other features
ridge <- glmnet(
  train[,-14], # feature matrix is all but last col
  train[,14], # response is the last col
  family = "gaussian" ,
  alpha = 1, # if alpha=0 that is ridge regression; if alpha = 1, that is LASSO
  lambda = 1
)
summary(ridge)
ridge_coeff <- ridge$beta
ridge_coeff

# find MSE on testing data






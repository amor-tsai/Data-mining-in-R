# to run single line on Mac: command + return, on Windows: control + return

# load dataset Boston (in MASS library)
library(MASS)
myData <- Boston

# split train/test datasets
set.seed(0)
trainIdx <- sample(nrow(myData), round(nrow(myData)*0.6)) # 60 % for training
train <- myData[trainIdx, ]
test <- myData[-trainIdx, ]

# install glmnet package
install.packages("glmnet")
library(glmnet)
?glmnet

# check data type
str(train)
train <- as.matrix(train)
test <- as.matrix(test)

# fit ridge regression on training data
# medv ~ other features
ridge <- glmnet(
  train[ ,-14], # feature matrix is all but last col
  train[ ,14], # response is the last col
  family = "gaussian",
  alpha = 0, # ridge regression
  lambda = 1
)

summary(ridge)
ridge_coeff <- ridge$beta
ridge_coeff

# find MSE on testing data
ridge_pred <- predict(ridge, test[ ,-14])
ridge_MSE <- mean((ridge_pred - test[ ,14])^2) # 24.22

# build LASSO model
lasso <- glmnet(
  train[ ,-14], # feature matrix is all but last col
  train[ ,14], # response is the last col
  family = "gaussian",
  alpha = 1, # lasso
  lambda = 1
)

lasso_coeff <- lasso$beta
lasso_coeff  


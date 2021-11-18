
# read data
myData <- read.csv("breast_cancer.csv")
head(myData) # 0: healthy, 1: cancer

# how many cancer patients?
table(myData$diagnosis)
barplot(table(myData$diagnosis),
        xlab = "0: healthy, 1: cancer",
        ylab = "Number of patients",
        ylim = c(0,400)) # range of y-axis
pie(table(myData$diagnosis), 
    c("healthy (37%)", "cancer (63%)"),
    main = "Distribution of patients")
table(myData$diagnosis)/nrow(myData) # percentage for each class
# check package ggplot for more plot options

# split data into train/test
set.seed(0)
trainIdx <- sample(nrow(myData), round(nrow(myData)*0.6))
train <- myData[trainIdx, ]
test <- myData[-trainIdx, ]

# use glm from MASS library
library(MASS)
?glm
m <- glm(diagnosis ~ ., 
         data = train,
         family = "binomial")
summary(m)

# misclassification error rate = (# incorrect pred)/(# data points)
# MSE (mean squared error) = sum((pred - true)^2)/(# data points)

# predict on training data
pr <- predict(m, train, type = "response") # sigmoid prob values
pr[1:10]
pred <- rep(0, nrow(train)) # a vector of zero
pred[pr > 0.5] <- 1 # if prob > 0.5, change prediction to 1
pred[1:10]
pr[1:10]

# misclassification error rate on training data
(pred != train$diagnosis) # num of not-matching values
error_rate = mean(pred != train$diagnosis) # 6% error

# misclassification error rate on testing data 
# ... 

# confusion matrix ( True Pos, True Neg, False Pos, False Neg)
table(pred, train$diagnosis)
#   train 0 , train 1
#   107,      7      , predicted as 0
#   13,       214    , predicted as 1






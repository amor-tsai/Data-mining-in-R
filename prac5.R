
myData <- read.csv("breast_cancer.csv")
head(myData)

table(myData$diagnosis)

barplot(table(myData$diagnosis),
         xlab = "0: healthy, 1:cancer",
         ylab = "Number of patients",
         ylim = c(0,400))

pie(table(myData$diagnosis)
    
    
    )

set.seed(0)
trainIdx <- sample(nrow(myData), round(nrow(myData)*0.6))
train <- myData[trainIdx, ]
test <- myData[-trainIdx, ]

library(MASS)
?glm
m <- glm(diagnosis ~ .,
         data = train,
         family = "binomial")
        
summary(m)

# misclassification error rate = (# incorrect pred)/(# data points)
# MSE (mean squared error) = sum(pred - true)^2)/(# data points)

#predict on training data
pr <- predict(m, train, type = "response") # sigmoid prob values
pr[1:10]
pred <- rep(0, nrow(train))
pred[pr >0.5] <- 1 # if prob > 0.5, change prediction to 1
pred[1:10]
pr[1:10]

# misclassification error rate on training data
(pred != train$diagnosis) # num of not-matching values
error_rate = mean(pred != train$diagnosis) # 6% error

# misclassification error rate on testing data

# confusion matrix ( True Pos, True Neg, False Pos, False Neg)
table(pred, train$diagnosis)
# train 0, train 1
#   107,      7,        predicted as 0
#   13,       214,      predicted as 1







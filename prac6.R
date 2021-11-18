# load breast_canser.csv

myData <- read.csv("breast_cancer.csv")
colnames(myData)
pairs(myData)

# Features: col1,2
# trim data
myData <- myData[,c(1,2,6)]
str(myData)


#change response type to Factor
myData$diagnosis <- as.factor((myData$diagnosis))
str(myData)

# split
trainIdx <- 1:400
train <- myData[trainIdx,]
test <- myData[-trainIdx,]

#check ratio between classes (0,1)
table(train$diagnosis)
table(test$diagnosis)

# K-nearest neighbor
#install.packages("FNN")

library(FNN)
?knn

pred <- knn(train[,1:2],test[,1:2],train$diagnosis,k=5)
pred[1:5]

#error rate
error_rate <- mean(pred!=test$diagnosis)

#confusion matrix
table(test$diagnosis,pred)

#plot of testing data
plot(test$mean_radius, test$mean_texture,
     col = test$diagnosis) # color 1:black, 2:red ...

plot(test$mean_radius, test$mean_texture,
     col = pred)



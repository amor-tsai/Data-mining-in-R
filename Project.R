#Camryn Maginel
#Guldman Kwak
#Liangchao Cai
#11/18/2021

?read.delim

#Download Data
# Here I use trainRaw as 
trainRaw <- read.delim("adult.data", header = FALSE, sep = ",")
colnames(trainRaw) <- c("age", "work-class", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")
testRaw <- read.delim("adult.test", header = FALSE, sep = ",")
testRaw<- testRaw[-1, ]
colnames(testRaw) <- c("age", "work-class", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country","income")

#Clean up the data

#remove native-country feature
train <- trainRaw[,-14]
test <- testRaw[,-14]

# separate income value from dataframe to y_train and y_test
y_train <- trainRaw[,15]
y_test <- testRaw[,15]

# remove income feature from dataframe
train <- train[,-15]
test <- test[,-15]

#Missing Values
train[train == " ?"] <- NA
train <- na.omit(train)
test[test == " ?"] <- NA
test <- na.omit(test)


#Duplicates
idxDup<-which(duplicated(train))
train <- train[-idxDup, ]
idxDup2 <- which(duplicated(test))
test <- test[-idxDup2, ]

#Outliers/Invalid Values
summary(train$income)

#Data Information
library(dplyr)
str(train)
str(test)
#install.packages("mltools")
library(mltools)
library(data.table)

# data pre-processing

#convert test$age into integer
test$age <- as.integer(test$age)

# define the function to do normalization
min_max_normalization <- function(x) {
  (x-min(x))/(max(x) - min(x))
}

# normalize the continous data
train <- mutate_if(train, is.integer, min_max_normalization)
test <- mutate_if(test, is.integer, min_max_normalization)

# normalize the age 
#train$age <- min_max_normalization(train$age)
#test$age <- min_max_normalization(test$age)

# normalize the fnlwgt
#train$fnlwgt <- min_max_normalization(train$fnlwgt)
#test$fnlwgt <- min_max_normalization(test$fnlwgt)

# normalize the education-num
#train$`education-num` <- min_max_normalization(train$`education-num`)
#test$`education-num` <- min_max_normalization(test$`education-num`)

?mutate_if

train <- mutate_if(train, is.character, as.factor)
test <- mutate_if(test, is.character, as.factor)

# I'not sure if it's better to do one-hot encoding or mutate to nominal, since after one-hot encoding,
# We may occur a problem to do dimension reduction.
train1 <- one_hot(as.data.table(train))
test1<-one_hot(as.data.table(test))

# mutate y data to nominal, but it starts from 1 instead 0, so we would get 1 and 2 instead of 0 and 1.
# It's  still a binary classification problem, though.
y_train <- as.numeric(as.factor(y_train))
y_test <- as.numeric(as.factor(y_test))

if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
library(RColorBrewer)

?corrplot
M <- cor(train1)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


#test1$nativecountryHolandNetherlands <- c(0)
#train1$`income_ >50K` <- c()
#test1$`income_ >50K.` <- c()

str(train1)
?as.factor
#train_income <- as.numeric(as.factor(train$income))

# Here maybe it shoud do MCA(Multiple Correspondence Analysis) instead of PCA(Principle Component Analysis)
# this code is only for PCA, but it may be not proper.
?prcomp
train1.pca <- prcomp(train1, center = TRUE,scale. = TRUE)
summary(train1.pca)

if (!require("devtools")) install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(train1.pca)


#KNN
library(FNN)
?knn
KNN_pred <- knn(train1, test1, train1$`income_ <=50K`, k = 1000)

#error rate
KNN_error_rate <- mean(KNN_pred!= test1$`income_ <=50K.`)

#confusion matrix
table(test1$`income_ <=50K.`, KNN_pred)


#plot of testing data


#Decision Tree (Creative Work: Bagging)
library("tree")
library("randomForest")
?tree
decisiontree <- tree(train$'income_ <=50K' ~ ., data = train1)

#Logistic Regression
?glm
LR <- glm(train1$`income_ <=50K` ~ .,
         data = train1,
         family = "binomial")
summary(LR)
pr_LR <- predict(LR, test1, type = "response")
pred_LR <- rep(1, nrow(test1))
pred_LR[pr_LR < .5] <- 0
error_rate_LR=mean(pred_LR != test1$`income_ <=50K.`)
pred_LR


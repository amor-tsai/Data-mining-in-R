#Camryn Maginel
#Guldman Kwak
#Liangchao Cai
#11/18/2021

#Download Data
train <- read.delim("adult.data", header = FALSE, sep = ",")
colnames(train) <- c("age", "work-class", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")
test <- read.delim("adult.test", header = FALSE, sep = ",")
test<- test[-1, ]
colnames(test) <- c("age", "work-class", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country","income")

#Clean up the data

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

#Outliers/Invalid Values/ Statistics
summary(train)
train1 <- train
outliersage <- boxplot(train1$age, plot = FALSE)$out
train1 <- train1[-which(train1$age %in% outliersage),]
outliersfnl <- boxplot(train1$fnlwgt, plot = FALSE)$out
train1 <- train1[-which(train1$fnlwgt %in% outliersfnl),]
outliersednum <- boxplot(train1$`education-num`, plot = FALSE)$out
train1 <- train1[-which(train1$`education-num` %in% outliersednum),]
outliersgain <- boxplot(train1$`capital-gain`, plot = FALSE)$out
train1 <- train1[-which(train1$`capital-gain` %in% outliersgain),]
outliersloss <- boxplot(train1$`capital-loss`, plot = FALSE)$out
train1 <- train1[-which(train1$`capital-loss` %in% outliersloss),]
outliershrs <- boxplot(train1$`hours-per-week`, plot = FALSE)$out
train1 <- train1[-which(train1$`hours-per-week` %in% outliershrs),]
train <- train1
test$age <- as.integer(test$age)
test1 <- test
testoutliersage <- boxplot(test1$age, plot = FALSE)$out
test1 <- test1[-which(test1$age %in% testoutliersage),]
testoutliersfnl <- boxplot(test1$fnlwgt, plot = FALSE)$out
test1 <- test1[-which(test1$fnlwgt %in% testoutliersfnl),]
testoutliersednum <- boxplot(test1$`education-num`, plot = FALSE)$out
test1 <- test1[-which(test1$`education-num` %in% testoutliersednum),]
testoutliersgain <- boxplot(test1$`capital-gain`, plot = FALSE)$out
test1 <- test1[-which(test1$`capital-gain` %in% testoutliersgain),]
testoutliersloss <- boxplot(test1$`capital-loss`, plot = FALSE)$out
test1 <- test1[-which(test1$`capital-loss` %in% testoutliersloss),]
testoutliershrs <- boxplot(test1$`hours-per-week`, plot = FALSE)$out
test1 <- test1[-which(test1$`hours-per-week` %in% testoutliershrs),]
test <- test1

#remove because all are equal to zero
train$`capital-gain` <- c()
train$`capital-loss` <-c()
test$`capital-gain` <- c()
test$`capital-loss`<-c()

#Range, Median and Mean
summary(train)

#variances
var(train$age)
var(train$`education-num`)
var(train$`hours-per-week`)

#Mode (most seen value)
mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode(train$age)
mode(train$`education-num`)
mode(train$`hours-per-week`)

#Frequency Counts
table(train$income)
table(train$`marital-status`)
table(train$relationship)
table(train$sex)

#visualize
hist(train$age, xlab = "Age", main = "Age Frequency", col = "#5891ad")
hist(train$`education-num`,xlab = "Education Years", main = "Years of Education Frequency", col = "#5891ad")
hist(train$`hours-per-week`, xlab = "Hours per Week", main = "Hours Per Week Frequency", col = "#5891ad")

#Relationships
plot(as.factor(train$relationship), as.factor(train$sex), xlab = "Relationship", ylab = "Sex", main = "Sex and Relationship Status", col = c("#5891ad", "#ff7f50" ))
plot(as.factor(train$`education-num`), as.factor(train$sex),col = c("#5891ad", "#ff7f50" ), xlab = "Education Years", ylab = "Sex", main = "Sex and Years of Education")

#To see the columns that are unlabeled in the first plot
table(train$sex, train$relationship)

#Data Information
library(dplyr)
str(train)
str(test)
#install.packages("mltools")
library(mltools)
library(data.table)

#Change to numerics
train <- mutate_if(train, is.character, as.factor)
train <- mutate_if(train, is.factor, as.numeric)
test <- mutate_if(test, is.character, as.factor)
test <- mutate_if(test, is.factor, as.numeric)


#Correlation Plot
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
?corrplot
M <- cor(train)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), main = "Adult Correlation Plot")

#KNN
library(FNN)
?knn
KNN_pred <- knn(train, test, train$income, k = 1000)
#error rate
KNN_error_rate <- mean(KNN_pred!= test$income)
#confusion matrix
table(test$income, KNN_pred)


#LDA
library(MASS)
mylda <- lda(income ~., data = train)
?lda
lda_Predict <- predict(mylda, test, type = "class")
lda_error <- mean(lda_Predict$class != test$income) 

#install.packages("sparseSVM")
library(sparseSVM)

#sparseSVM
x <- as.matrix(train[, -13])
y <- as.matrix(train[ , 13])
xtest <- as.matrix(test[, -13])
ytest <- as.matrix(test[ , 13])
sparseSVM <- sparseSVM(x, y)
coef(sparseSVM, .05)
sparse_pred <- predict(sparseSVM, xtest, lambda = c(0.2, 0.1))
sparse_error <- mean(sparse_pred != test$income)

#Remove columns that SVM Sparse finds unimportant
train$`work-class` <- c()
test$`work-class` <- c()
train$fnlwgt <- c()
test$fnlwgt <-c()
train$education <- c()
test$education <- c()
train$occupation <- c()
test$occupation <- c()
train$race <- c()
test$race <- c()
train$`native-country` <- c()
test$`native-country` <- c()

#KNN
library(FNN)
?knn
Important_KNN_pred <- knn(train, test, train$income, k = 1000)
#error rate
Important_KNN_error_rate <- mean(Important_KNN_pred!= test$income)
#confusion matrix
table(test$income, Important_KNN_pred)

#LDA
library(MASS)
importantmylda <- lda(income ~., data = train)
importantlda_Predict <- predict(importantmylda, test, type = "class")
importantlda_error <- mean(importantlda_Predict$class != test$income) 

#Test now with one hot encoding the important features
#Download Data
train <- read.delim("adult.data", header = FALSE, sep = ",")
colnames(train) <- c("age", "work-class", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")
test <- read.delim("adult.test", header = FALSE, sep = ",")
test<- test[-1, ]
colnames(test) <- c("age", "work-class", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country","income")

#Clean up the data

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
#Outliers/Invalid Values/ Statistics
summary(train)
train1 <- train
outliersage <- boxplot(train1$age, plot = FALSE)$out
train1 <- train1[-which(train1$age %in% outliersage),]
outliersfnl <- boxplot(train1$fnlwgt, plot = FALSE)$out
train1 <- train1[-which(train1$fnlwgt %in% outliersfnl),]
outliersednum <- boxplot(train1$`education-num`, plot = FALSE)$out
train1 <- train1[-which(train1$`education-num` %in% outliersednum),]
outliersgain <- boxplot(train1$`capital-gain`, plot = FALSE)$out
train1 <- train1[-which(train1$`capital-gain` %in% outliersgain),]
outliersloss <- boxplot(train1$`capital-loss`, plot = FALSE)$out
train1 <- train1[-which(train1$`capital-loss` %in% outliersloss),]
outliershrs <- boxplot(train1$`hours-per-week`, plot = FALSE)$out
train1 <- train1[-which(train1$`hours-per-week` %in% outliershrs),]
train <- train1
test$age <- as.integer(test$age)
test1 <- test
testoutliersage <- boxplot(test1$age, plot = FALSE)$out
test1 <- test1[-which(test1$age %in% testoutliersage),]
testoutliersfnl <- boxplot(test1$fnlwgt, plot = FALSE)$out
test1 <- test1[-which(test1$fnlwgt %in% testoutliersfnl),]
testoutliersednum <- boxplot(test1$`education-num`, plot = FALSE)$out
test1 <- test1[-which(test1$`education-num` %in% testoutliersednum),]
testoutliersgain <- boxplot(test1$`capital-gain`, plot = FALSE)$out
test1 <- test1[-which(test1$`capital-gain` %in% testoutliersgain),]
testoutliersloss <- boxplot(test1$`capital-loss`, plot = FALSE)$out
test1 <- test1[-which(test1$`capital-loss` %in% testoutliersloss),]
testoutliershrs <- boxplot(test1$`hours-per-week`, plot = FALSE)$out
test1 <- test1[-which(test1$`hours-per-week` %in% testoutliershrs),]
test <- test1

#remove because all are equal to zero
train$`capital-gain` <- c()
train$`capital-loss` <-c()
test$`capital-gain` <- c()
test$`capital-loss`<-c()


#Data Information
library(dplyr)
str(train)
str(test)
#install.packages("mltools")
library(mltools)
library(data.table)

#remove unimportant features
train$`work-class` <- c()
test$`work-class` <- c()
train$fnlwgt <- c()
test$fnlwgt <-c()
train$education <- c()
test$education <- c()
train$occupation <- c()
test$occupation <- c()
train$race <- c()
test$race <- c()
train$`native-country` <- c()
test$`native-country` <- c()

#One hot
train <- mutate_if(train, is.character, as.factor)
test$age <- as.integer(test$age)
test <- mutate_if(test, is.character, as.factor)
train <- as.data.table(train[ ])
train <- one_hot(train[])
test<-one_hot(as.data.table(test[ ]))

#remove the 2nd response column
train <- train[ , -20]
test <- test[,-20]

library(e1071)
?svm
hotsvm<- svm(train$`income_ <=50K`~.,data=train)
Hot_svm_Predict <- sign(predict(hotsvm, test, type = "class"))
Hot_svm_error <- mean(Hot_svm_Predict != test$`income_ <=50K.`)

#KNN
library(FNN)
?knn
#Predict
Hot_KNN_pred <- knn(train, test, train$`income_ <=50K`, k = 1000)
#error rate
Hot_KNN_error_rate <- mean(Hot_KNN_pred!= test$`income_ <=50K.`)
#confusion matrix
table(test$`income_ <=50K.` , Hot_KNN_pred)

#LDA
library(MASS)
Hot_mylda <- lda(train$`income_ <=50K` ~., data = train)
Hot_lda_Predict <- predict(Hot_mylda, test, type = "class")
Hot_lda_error <- mean(Hot_lda_Predict$class != test$`income_ <=50K.`)


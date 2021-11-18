
myData <- read.csv("abalone.csv", header = TRUE)
myData <- read.delim2("abalone.csv", sep = ",")

str(myData)

myData$Gender <- as.factor(myData$Gender)

# check ratio I/F/M
summary(myData$Gender)
table(myData$Gender)

set.seed(0)
library(sampling)
?strata

st_output <- strata(myData, stratanames = "Gender", size = c(1000,1000,1000))
st_output

myTrain <- myData[st_output$ID_unit, ]
myTest <- myData[-st_output$ID_unit, ]

# naive bayes
if (!require("naivebayes")) install.packages("naivebayes")
library("naivebayes")
?naive_bayes
nb <- naive_bayes(Gender ~ ., myTrain)
summary(nb)
nb_pred <- predict(nb, myTest, type = "class")
table(nb_pred, myTest$Gender)
mean(nb_pred != myTest$Gender)


#LDA
library(MASS)
lda <- lda(Gender ~., myTrain)
lda_pred <- predict(lda, myTest)
lda_pred$class # your prediction
mean(lda_pred$class != myTest$Gender)

#SVM

if (!require("e1071")) install.packages("e1071")
library(e1071)

#remove Infant class
infant_idx <- which(myTrain$Gender == "I") # T or F for each entry
myTrain <- myTrain[-infant_idx, ]
myTest <- myTest[-infant_idx, ]

svm_model <- svm(Gender ~. , myTrain, kernel = "linear")
svm_pred <- predict(svm_model, myTest)
mean(svm_pred != myTest$Gender)





#install.packages("MASS")
#install.packages("caret")
library("MASS")

myData <- Cars93

?str()
#3.check data type of each column by using str() function
str(myData)

#4.Are there any empty values (NA) in the dataset? (Hint: use summary( ) function.) 
#If there are, remove rows containing empty values by using na.omit( ).
?summary()
summary(myData)
# yes, the Rear.seat.room and Luggage.room have NA data.
?na.omit()
#remove the NA data
myData <-na.omit(myData)

summary(myData)
#5.Remove all the columns of ‘Factor’ type.

?which
?sapply
myData <- myData[,-which(sapply(myData, class) == "factor")]

str(myData)
#6.Split the data into training and testing sets. Assign ﬁrst 60 rows as the training set, and the rest for the testing set.

data_train <- head(myData,n=60)
data_test <- tail(myData,n=22)
#7. Build a linear model to predict ‘Horsepower’ using training data. Write down the linear model.

?lm

m1 <- lm(Horsepower ~ ., data=data_train)
summary(m1)

#8. Which features are signiﬁcant?

library("caret")
lm1Imp <- varImp(m1)
lm1Imp
# RPM, Weight and EngineSize are significant features
print("RPM, Weight and EngineSize are significant features")

#9. Make prediction on testing data. What is the mean squared error?

?predict
x_test <- data_test[, !(names(data_test) %in% c("Horsepower"))]
y_test <- data_test$Horsepower

fit <- predict(m1, x_test)

mean((y_test - fit)^2)

#10.Can you increase the model accuracy by removing some of the insigniﬁcant features from the model?

#I could remove some insignificant features such as Rev.per.mile,Turn.circle and Rear.seat.room
